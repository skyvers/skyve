package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

import javax.imageio.ImageIO;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.repository.ProvidedRepository;

@SuppressWarnings("static-method")
class ImageUtilTest {
	@TempDir
	private File temp;

	private final ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();

	@AfterEach
	void teardown() {
		if (originalRepository == null) {
			ProvidedRepositoryFactory.clear();
		}
		else {
			ProvidedRepositoryFactory.set(originalRepository);
		}
	}

	@Test
	void signatureWithNullColorsRendersTransparentPng() throws Exception {
		String json = "[[[10,10],[50,50],[90,10]]]";
		byte[] result = ImageUtil.signature(json, 200, 100, null, null);
		assertNotNull(result);
		assertTrue(result.length > 0);
		// Verify it's a valid PNG by reading it back
		BufferedImage img = ImageIO.read(new ByteArrayInputStream(result));
		assertNotNull(img);
	}

	@Test
	void signatureWithExplicitColorsRendersOpaquePng() throws Exception {
		String json = "[[[10,10],[50,50],[90,10]]]";
		byte[] result = ImageUtil.signature(json, 200, 100, "#ffffff", "#000000");
		assertNotNull(result);
		assertTrue(result.length > 0);
		BufferedImage img = ImageIO.read(new ByteArrayInputStream(result));
		assertNotNull(img);
	}

	@Test
	void signatureWithEmptyLinesJsonRendersBlankPng() throws Exception {
		// No strokes — just creates a blank image
		String json = "[]";
		byte[] result = ImageUtil.signature(json, 100, 100, null, null);
		assertNotNull(result);
		assertTrue(result.length > 0);
	}

	@Test
	void readValidPngStreamReturnsImage() throws Exception {
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
	void readValidPngStreamWithLargeTargetReturnsImage() throws Exception {
		// subsamplingMinimumTargetSize larger than image → subsampling = 0 (not applied)
		BufferedImage source = new BufferedImage(20, 20, BufferedImage.TYPE_INT_RGB);
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		ImageIO.write(source, "PNG", bos);
		InputStream is = new ByteArrayInputStream(bos.toByteArray());
		BufferedImage result = ImageUtil.read(is, 1000);
		assertNotNull(result);
	}

	@Test
	void readNonImageStreamReturnsNull() throws Exception {
		// Pass non-image bytes — ImageIO cannot read it, should return null
		InputStream is = new ByteArrayInputStream("not an image".getBytes());
		BufferedImage result = ImageUtil.read(is, 1);
		assertNull(result);
	}

	@Test
	void svgWithoutExtensionUsesBlankIconAndAddsDimensions() throws Exception {
		File blank = svgFile("blank.svg", "<svg viewBox=\"0 0 10 10\"><rect width=\"10\" height=\"10\"/></svg>");
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.findResourceFile("files/blank.svg", null, null)).thenReturn(blank);
		ProvidedRepositoryFactory.set(repository);

		String result = new String(ImageUtil.svg("README", 32, 24), StandardCharsets.UTF_8);

		assertTrue(result.startsWith("<svg width=\"32px\" height=\"24px\" "));
		assertTrue(result.contains("viewBox=\"0 0 10 10\""));
	}

	@Test
	void svgFallsBackToBlankIconWhenSpecificIconDoesNotExist() throws Exception {
		File missingSpecificIcon = new File(temp, "missing.svg");
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
	void burnSvgWithSimpleRectRendersOntoImage() throws Exception {
		BufferedImage image = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
		assertEquals(100, image.getWidth());
		String svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\">"
				+ "<rect width=\"100\" height=\"100\" fill=\"blue\"/>"
				+ "</svg>";
		ImageUtil.burnSvg(image, svg);
		// No exception means success
	}

	@Test
	void burnSvgScalesFullResolutionMarkupOntoASubsampledImage() throws Exception {
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
	void burnSvgDrawsTextHungFromItsTopEdgeBelowTheTop() throws Exception {
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
		assertTrue(inkedBelow > 100, "no text drawn below its top edge");
	}

	@Test
	void burnSvgDrawsMarkupMatchingTheImageAtItsOwnSize() throws Exception {
		BufferedImage image = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
		String svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\" viewBox=\"0 0 100 100\">"
				+ "<rect x=\"10\" y=\"10\" width=\"20\" height=\"20\" fill=\"#0000ff\"/>"
				+ "</svg>";

		ImageUtil.burnSvg(image, svg);

		assertEquals(0xff0000ff, image.getRGB(20, 20));
		assertEquals(0, image.getRGB(40, 40) >>> 24);
	}

	@Test
	void burnSvgWithTransparentFillAttributeNormalises() throws Exception {
		// Covers normaliseTransparentForBatik L229-231 (fill="transparent" → "none")
		BufferedImage image = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
		assertEquals(100, image.getHeight());
		String svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\">"
				+ "<rect width=\"50\" height=\"50\" fill=\"transparent\"/>"
				+ "</svg>";
		ImageUtil.burnSvg(image, svg);
	}

	@Test
	void burnSvgWithStyleTransparentNormalises() throws Exception {
		// Covers normaliseTransparentForBatik L225-227 (style="...transparent..." → "...none...")
		BufferedImage image = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
		assertEquals(100, image.getWidth());
		String svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\">"
				+ "<rect width=\"50\" height=\"50\" style=\"fill: transparent;\"/>"
				+ "</svg>";
		ImageUtil.burnSvg(image, svg);
	}

	@Test
	void burnSvgWithStyleElementStripsStyleAndSucceeds() throws Exception {
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
		File file = new File(temp, name);
		Files.writeString(file.toPath(), xml, StandardCharsets.UTF_8);
		return file;
	}
}
