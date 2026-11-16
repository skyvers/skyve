package org.skyve.impl.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.zip.CRC32;

import javax.imageio.ImageIO;

import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.repository.ProvidedRepository;

@SuppressWarnings("static-method")
public class ImageUtilTest {
	@Rule
	public TemporaryFolder temp = new TemporaryFolder();

	private final ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();

	@After
	public void teardown() {
		if (originalRepository == null) {
			ProvidedRepositoryFactory.clear();
		}
		else {
			ProvidedRepositoryFactory.set(originalRepository);
		}
	}

	@Test
	public void signatureWithNullColorsRendersTransparentPng() throws Exception {
		String json = "[[[10,10],[50,50],[90,10]]]";
		byte[] result = ImageUtil.signature(json, 200, 100, null, null);
		assertNotNull(result);
		assertTrue(result.length > 0);
		// Verify it's a valid PNG by reading it back
		BufferedImage img = ImageIO.read(new ByteArrayInputStream(result));
		assertNotNull(img);
	}

	@Test
	public void signatureWithExplicitColorsRendersOpaquePng() throws Exception {
		String json = "[[[10,10],[50,50],[90,10]]]";
		byte[] result = ImageUtil.signature(json, 200, 100, "#ffffff", "#000000");
		assertNotNull(result);
		assertTrue(result.length > 0);
		BufferedImage img = ImageIO.read(new ByteArrayInputStream(result));
		assertNotNull(img);
	}

	@Test
	public void signatureWithEmptyLinesJsonRendersBlankPng() throws Exception {
		// No strokes — just creates a blank image
		String json = "[]";
		byte[] result = ImageUtil.signature(json, 100, 100, null, null);
		assertNotNull(result);
		assertTrue(result.length > 0);
	}

	@Test
	public void readValidPngStreamReturnsImage() throws Exception {
		// Create a small PNG in-memory
		BufferedImage source = new BufferedImage(20, 20, BufferedImage.TYPE_INT_RGB);
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		ImageIO.write(source, "PNG", bos);
		InputStream is = new ByteArrayInputStream(bos.toByteArray());
		// subsamplingMinimumTargetSize = 1 → subsampling = 20/1 = 20 > 1
		BufferedImage result = ImageUtil.read(is, 1);
		assertNotNull(result);
	}

	@Test
	public void readValidPngStreamWithLargeTargetReturnsImage() throws Exception {
		// subsamplingMinimumTargetSize larger than image → subsampling = 0 (not applied)
		BufferedImage source = new BufferedImage(20, 20, BufferedImage.TYPE_INT_RGB);
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		ImageIO.write(source, "PNG", bos);
		InputStream is = new ByteArrayInputStream(bos.toByteArray());
		BufferedImage result = ImageUtil.read(is, 1000);
		assertNotNull(result);
	}

	@Test
	public void readNonImageStreamReturnsNull() throws Exception {
		// Pass non-image bytes — ImageIO cannot read it, should return null
		InputStream is = new ByteArrayInputStream("not an image".getBytes());
		BufferedImage result = ImageUtil.read(is, 1);
		assertNull(result);
	}

	@Test
	public void svgWithoutExtensionUsesBlankIconAndAddsDimensions() throws Exception {
		File blank = svgFile("blank.svg", "<svg viewBox=\"0 0 10 10\"><rect width=\"10\" height=\"10\"/></svg>");
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.findResourceFile("files/blank.svg", null, null)).thenReturn(blank);
		ProvidedRepositoryFactory.set(repository);

		String result = new String(ImageUtil.svg("README", 32, 24), StandardCharsets.UTF_8);

		// The square icon is the largest square inside 32x24
		assertTrue(result, result.startsWith("<svg width=\"24px\" height=\"24px\" "));
		assertTrue(result.contains("viewBox=\"0 0 10 10\""));
	}

	@Test
	public void svgKeepsTheShapeOfItsViewBoxInsideTheRequestedBox() throws Exception {
		File pdf = svgFile("pdf.svg", "<svg id=\"Layer_1\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 72 96\"><path d=\"M0 0h72v96H0z\"/></svg>");
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.findResourceFile("files/pdf.svg", null, null)).thenReturn(pdf);
		ProvidedRepositoryFactory.set(repository);

		assertTrue(new String(ImageUtil.svg("a.pdf", 512, 512), StandardCharsets.UTF_8).startsWith("<svg width=\"384px\" height=\"512px\" "));
		assertTrue(new String(ImageUtil.svg("a.pdf", 300, 150), StandardCharsets.UTF_8).startsWith("<svg width=\"113px\" height=\"150px\" "));
		assertTrue(new String(ImageUtil.svg("a.pdf", 120, 300), StandardCharsets.UTF_8).startsWith("<svg width=\"120px\" height=\"160px\" "));
	}

	@Test
	public void svgWithoutAUsableViewBoxUsesTheRequestedBox() throws Exception {
		File noViewBox = svgFile("txt.svg", "<svg xmlns=\"http://www.w3.org/2000/svg\"><rect width=\"10\" height=\"10\"/></svg>");
		File malformed = svgFile("csv.svg", "<svg viewBox=\"0 0 wide tall\"><rect width=\"10\" height=\"10\"/></svg>");
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.findResourceFile("files/txt.svg", null, null)).thenReturn(noViewBox);
		when(repository.findResourceFile("files/csv.svg", null, null)).thenReturn(malformed);
		ProvidedRepositoryFactory.set(repository);

		assertTrue(new String(ImageUtil.svg("a.txt", 64, 32), StandardCharsets.UTF_8).startsWith("<svg width=\"64px\" height=\"32px\" "));
		assertTrue(new String(ImageUtil.svg("a.csv", 64, 32), StandardCharsets.UTF_8).startsWith("<svg width=\"64px\" height=\"32px\" "));
	}

	@Test
	public void readWithABoxSubsamplesNoSmallerThanTheBoxNeeds() throws Exception {
		BufferedImage source = new BufferedImage(2000, 1500, BufferedImage.TYPE_INT_RGB);
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		ImageIO.write(source, "PNG", bos);
		byte[] png = bos.toByteArray();

		// Without a box, 1500 / 512 = 2, so the image is read at half size
		assertEquals("1000x750", readSize(png, 0, 0));
		// A 256 box is decoded at up to twice that - 2000 / 512 = 3.9, rounded up to 4
		assertEquals("500x375", readSize(png, 256, 256));
		// A 1024 box is decoded at up to 2048, so the image is read at full size
		assertEquals("2000x1500", readSize(png, 1024, 1024));
		// A box only constrains when both its sides are given
		assertEquals("1000x750", readSize(png, 1024, 0));
	}

	@Test
	public void readNeverDecodesMorePixelsThanTheBudget() throws Exception {
		BufferedImage source = new BufferedImage(2000, 1500, BufferedImage.TYPE_INT_RGB);
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		ImageIO.write(source, "PNG", bos);
		byte[] png = bos.toByteArray();

		int originalMaximum = UtilImpl.THUMBNAIL_MAXIMUM_DIMENSION;
		try {
			// The budget is (2 x 100) squared = 40,000 pixels, so 3,000,000 pixels are sub-sampled by 9
			UtilImpl.THUMBNAIL_MAXIMUM_DIMENSION = 100;
			assertEquals("223x167", readSize(png, 0, 0));
			// A 100 box already sub-samples by 20, which is within the budget
			assertEquals("100x75", readSize(png, 100, 100));
		}
		finally {
			UtilImpl.THUMBNAIL_MAXIMUM_DIMENSION = originalMaximum;
		}
	}

	@Test
	public void readWithABoxDecodesBetweenOnceAndTwiceTheBoxSize() throws Exception {
		// 1023 / 512 = 1.998, rounded up to 2, so a 256 box is decoded at 512 and not at full size
		assertEquals("512x512", readSize(png(1023, 1023), 256, 256));
		// Rounding up would decode 2100 / 2 = 1050, smaller than a 2048 box needs, so it is not sub-sampled
		assertEquals("2100x10", readSize(png(2100, 10), 2048, 2048));
	}

	private static byte[] png(int width, int height) throws Exception {
		BufferedImage source = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		ImageIO.write(source, "PNG", bos);
		return bos.toByteArray();
	}

	@Test
	public void readDoesNotDecodeAnImageOfMoreThan256Megapixels() throws Exception {
		// A 1x1 PNG whose header claims 16385 x 16384 pixels - the decoder would allocate for the header's size
		BufferedImage source = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		ImageIO.write(source, "PNG", bos);
		byte[] png = bos.toByteArray();
		ByteBuffer header = ByteBuffer.wrap(png);
		header.putInt(16, 16385); // IHDR width
		header.putInt(20, 16384); // IHDR height
		CRC32 crc = new CRC32();
		crc.update(png, 12, 17); // "IHDR" and its 13 data bytes
		header.putInt(29, (int) crc.getValue());

		assertNull(ImageUtil.read(new ByteArrayInputStream(png), 512));
	}

	@Test
	public void thumbnailStepsDoubleFrom64UpToTheMaximum() {
		assertEquals(64, ImageUtil.thumbnailStep(1));
		assertEquals(64, ImageUtil.thumbnailStep(64));
		assertEquals(128, ImageUtil.thumbnailStep(65));
		assertEquals(256, ImageUtil.thumbnailStep(256));
		assertEquals(512, ImageUtil.thumbnailStep(257));
		assertEquals(2048, ImageUtil.thumbnailStep(1025));
		assertEquals(2048, ImageUtil.thumbnailStep(Integer.MAX_VALUE));

		int originalMaximum = UtilImpl.THUMBNAIL_MAXIMUM_DIMENSION;
		try {
			UtilImpl.THUMBNAIL_MAXIMUM_DIMENSION = 1000;
			assertEquals(1000, ImageUtil.thumbnailStep(600));
			UtilImpl.THUMBNAIL_MAXIMUM_DIMENSION = 50;
			assertEquals(50, ImageUtil.thumbnailStep(24));
		}
		finally {
			UtilImpl.THUMBNAIL_MAXIMUM_DIMENSION = originalMaximum;
		}
	}

	@Test
	public void thumbnailSizeIsTheLargerSideRoundedUpToAStep() {
		assertEquals(0, ImageUtil.thumbnailSize(0, 0));
		assertEquals(0, ImageUtil.thumbnailSize(-1, -5));
		assertEquals(64, ImageUtil.thumbnailSize(24, 24));
		assertEquals(64, ImageUtil.thumbnailSize(24, 0));
		assertEquals(64, ImageUtil.thumbnailSize(0, 24));
		assertEquals(512, ImageUtil.thumbnailSize(300, 40));
		assertEquals(2048, ImageUtil.thumbnailSize(2047, 1));
		assertEquals(2048, ImageUtil.thumbnailSize(99_999, 1));
	}

	private static String readSize(byte[] png, int fitWidth, int fitHeight) throws Exception {
		BufferedImage image = ImageUtil.read(new ByteArrayInputStream(png), 512, fitWidth, fitHeight);
		if (image == null) {
			throw new AssertionError("The PNG was not read");
		}
		return image.getWidth() + "x" + image.getHeight();
	}

	@Test
	public void svgFallsBackToBlankIconWhenSpecificIconDoesNotExist() throws Exception {
		File missingSpecificIcon = new File(temp.getRoot(), "missing.svg");
		File blank = svgFile("blank.svg", "<svg viewBox=\"0 0 20 20\"><circle r=\"4\"/></svg>");
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.findResourceFile("files/pdf.svg", null, null)).thenReturn(missingSpecificIcon);
		when(repository.findResourceFile("files/blank.svg", null, null)).thenReturn(blank);
		ProvidedRepositoryFactory.set(repository);

		String result = new String(ImageUtil.svg("REPORT.PDF", 48, 48), StandardCharsets.UTF_8);

		assertTrue(result.startsWith("<svg width=\"48px\" height=\"48px\" "));
		assertTrue(result.contains("viewBox=\"0 0 20 20\""));
	}

	@Test
	public void burnSvgWithSimpleRectRendersOntoImage() throws Exception {
		BufferedImage image = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
		assertEquals(100, image.getWidth());
		String svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\">"
				+ "<rect width=\"100\" height=\"100\" fill=\"blue\"/>"
				+ "</svg>";
		ImageUtil.burnSvg(image, svg);
		// No exception means success
	}

	@Test
	public void burnSvgScalesFullResolutionMarkupOntoASubsampledImage() throws Exception {
		// Markup drawn against a 400px image, burned onto a 100px copy of it
		BufferedImage image = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
		String svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"400\" height=\"400\" viewBox=\"0 0 400 400\">"
				+ "<rect x=\"200\" y=\"200\" width=\"200\" height=\"200\" fill=\"#ff0000\"/>"
				+ "</svg>";

		ImageUtil.burnSvg(image, svg);

		assertEquals(0xffff0000, image.getRGB(75, 75));
		assertEquals(0xffff0000, image.getRGB(55, 95));
		assertEquals(0, image.getRGB(25, 25) >>> 24);
		assertEquals(0, image.getRGB(45, 75) >>> 24);
	}

	@Test
	public void burnSvgDrawsTextHungFromItsTopEdgeBelowTheTop() throws Exception {
		// Excalidraw positions text by its top edge: y="0" with dominant-baseline="text-before-edge"
		BufferedImage image = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
		String svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\" viewBox=\"0 0 100 100\">"
				+ "<g transform=\"translate(10 30)\"><text x=\"0\" y=\"0\" font-family=\"Helvetica\" font-size=\"40px\" fill=\"#000000\" "
				+ "text-anchor=\"start\" dominant-baseline=\"text-before-edge\">HH</text></g>"
				+ "</svg>";

		ImageUtil.burnSvg(image, svg);

		// The letters hang below y=30, so they are inked between 30 and 75 and not above
		int inkedAbove = 0;
		int inkedBelow = 0;
		for (int y = 0; y < 100; y++) {
			for (int x = 0; x < 100; x++) {
				if ((image.getRGB(x, y) >>> 24) > 128) {
					if (y < 30) {
						inkedAbove++;
					}
					else {
						inkedBelow++;
					}
				}
			}
		}
		assertEquals(0, inkedAbove);
		assertTrue("no text drawn below its top edge", inkedBelow > 100);
	}

	@Test
	public void burnSvgDrawsMarkupMatchingTheImageAtItsOwnSize() throws Exception {
		BufferedImage image = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
		String svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\" viewBox=\"0 0 100 100\">"
				+ "<rect x=\"10\" y=\"10\" width=\"20\" height=\"20\" fill=\"#0000ff\"/>"
				+ "</svg>";

		ImageUtil.burnSvg(image, svg);

		assertEquals(0xff0000ff, image.getRGB(20, 20));
		assertEquals(0, image.getRGB(40, 40) >>> 24);
	}

	@Test
	public void burnSvgWithTransparentFillAttributeNormalises() throws Exception {
		// Covers normaliseTransparentForBatik L229-231 (fill="transparent" → "none")
		BufferedImage image = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
		assertEquals(100, image.getHeight());
		String svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\">"
				+ "<rect width=\"50\" height=\"50\" fill=\"transparent\"/>"
				+ "</svg>";
		ImageUtil.burnSvg(image, svg);
	}

	@Test
	public void burnSvgWithStyleTransparentNormalises() throws Exception {
		// Covers normaliseTransparentForBatik L225-227 (style="...transparent..." → "...none...")
		BufferedImage image = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
		assertEquals(100, image.getWidth());
		String svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\">"
				+ "<rect width=\"50\" height=\"50\" style=\"fill: transparent;\"/>"
				+ "</svg>";
		ImageUtil.burnSvg(image, svg);
	}

	@Test
	public void burnSvgWithStyleElementStripsStyleAndSucceeds() throws Exception {
		// Covers the replaceAll for <style> element stripping in burnSvg
		BufferedImage image = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
		assertEquals(100, image.getHeight());
		String svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\">"
				+ "<style>.cls1{fill:red}</style>"
				+ "<rect width=\"50\" height=\"50\" fill=\"red\"/>"
				+ "</svg>";
		ImageUtil.burnSvg(image, svg);
	}

	private File svgFile(String name, String xml) throws Exception {
		File file = temp.newFile(name);
		Files.writeString(file.toPath(), xml, StandardCharsets.UTF_8);
		return file;
	}
}
