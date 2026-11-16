package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertThrowsExactly;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Stream;

import javax.imageio.ImageIO;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.content.AttachmentContent;
import org.skyve.content.MimeType;
import org.skyve.impl.util.UtilImpl;

@SuppressWarnings("static-method")
class ThumbnailTest {

	@TempDir
	File tempDir;

	/** Saves and restores THUMBNAIL_DIRECTORY so tests don't affect each other. */
	private String savedThumbnailDirectory;
	private boolean savedThumbnailFileStorage;

	@BeforeEach
	void setUp() {
		savedThumbnailDirectory = UtilImpl.THUMBNAIL_DIRECTORY;
		savedThumbnailFileStorage = UtilImpl.THUMBNAIL_FILE_STORAGE;
		// Point thumbnails at our temp dir so Thumbnail(File,int,int) can write
		UtilImpl.THUMBNAIL_DIRECTORY = tempDir.getAbsolutePath() + "/thumbnails/";
		UtilImpl.THUMBNAIL_FILE_STORAGE = true;
	}

	@AfterEach
	void tearDown() {
		UtilImpl.THUMBNAIL_DIRECTORY = savedThumbnailDirectory;
		UtilImpl.THUMBNAIL_FILE_STORAGE = savedThumbnailFileStorage;
	}

	/** Writes a minimal 2×2 PNG to a temp file and returns it. */
	private File createPngFile() throws IOException {
		BufferedImage img = new BufferedImage(2, 2, BufferedImage.TYPE_INT_RGB);
		Graphics2D g = img.createGraphics();
		g.setColor(Color.RED);
		g.fillRect(0, 0, 2, 2);
		g.dispose();
		File png = new File(tempDir, "test.png");
		ImageIO.write(img, "png", png);
		return png;
	}

	/** Writes a small SVG text to a temp file and returns it. */
	private File createSvgFile() throws IOException {
		File svg = new File(tempDir, "test.svg");
		Files.write(svg.toPath(), "<svg xmlns=\"http://www.w3.org/2000/svg\"><rect width=\"10\" height=\"10\"/></svg>".getBytes());
		return svg;
	}

	/** Writes some arbitrary bytes to a temp file named *.txt and returns it. */
	private File createTextFile() throws IOException {
		File txt = new File(tempDir, "test.txt");
		Files.write(txt.toPath(), "hello world".getBytes());
		return txt;
	}

	@Test
	void fileConstructorReadsBytesFromPngFile() throws IOException {
		File pngFile = createPngFile();
		Thumbnail t = new Thumbnail(pngFile);
		assertNotNull(t.getBytes());
		// bytes should be the exact PNG content
		assertArrayEquals(Files.readAllBytes(pngFile.toPath()), t.getBytes());
	}

	@Test
	void fileConstructorDetectsPngMimeType() throws IOException {
		File pngFile = createPngFile();
		Thumbnail t = new Thumbnail(pngFile);
		assertEquals(MimeType.png, t.getMimeType());
	}

	@Test
	void fileConstructorReadsBytesFromSvgFile() throws IOException {
		File svgFile = createSvgFile();
		Thumbnail t = new Thumbnail(svgFile);
		assertNotNull(t.getBytes());
		assertArrayEquals(Files.readAllBytes(svgFile.toPath()), t.getBytes());
	}

	@Test
	void fileConstructorDetectsSvgMimeType() throws IOException {
		File svgFile = createSvgFile();
		Thumbnail t = new Thumbnail(svgFile);
		assertEquals(MimeType.svg, t.getMimeType());
	}

	@Test
	void storedFindsTheThumbnailTheInputStreamConstructorStored() throws IOException {
		byte[] png = Files.readAllBytes(createPngFile("streamed.png", 400, 300).toPath());
		assertNull(Thumbnail.stored("/images/streamed.png", "image/png", 128, 128));

		Thumbnail made = new Thumbnail(new ByteArrayInputStream(png), "/images/streamed.png", "image/png", 128, 128);
		Thumbnail stored = Thumbnail.stored("/images/streamed.png", "image/png", 128, 128);

		assertNotNull(stored);
		assertArrayEquals(made.getBytes(), stored.getBytes());
		assertEquals(MimeType.png, stored.getMimeType());
		// Another size still has to be made
		assertNull(Thumbnail.stored("/images/streamed.png", "image/png", 256, 256));
	}

	@Test
	void storedFindsNothingWithoutFileStorage() throws IOException {
		UtilImpl.THUMBNAIL_FILE_STORAGE = false;
		byte[] png = Files.readAllBytes(createPngFile("unstored.png", 400, 300).toPath());
		assertNotNull(new Thumbnail(new ByteArrayInputStream(png), "/images/unstored.png", "image/png", 128, 128).getBytes());

		assertNull(Thumbnail.stored("/images/unstored.png", "image/png", 128, 128));
	}

	@Test
	void fileConstructorNonImageFileHasMimeTypeOrNull() throws IOException {
		File txtFile = createTextFile();
		Thumbnail t = new Thumbnail(txtFile);
		assertNotNull(t.getBytes());
		// .txt may or may not have a known MimeType depending on Skyve's enum; just verify bytes are present
		assertArrayEquals(Files.readAllBytes(txtFile.toPath()), t.getBytes());
	}

	@Test
	void getBytesReturnsNonEmptyArrayForPng() throws IOException {
		File pngFile = createPngFile();
		Thumbnail t = new Thumbnail(pngFile);
		byte[] bytes = t.getBytes();
		assertNotNull(bytes);
		// A PNG always starts with the 8-byte signature
		assertEquals((byte) 0x89, bytes[0]);
		assertEquals((byte) 'P', bytes[1]);
		assertEquals((byte) 'N', bytes[2]);
		assertEquals((byte) 'G', bytes[3]);
	}

	// ---- AttachmentContent constructor (width=0, height=0) ----

	private AttachmentContent createPngContent() throws IOException {
		BufferedImage img = new BufferedImage(2, 2, BufferedImage.TYPE_INT_RGB);
		Graphics2D g = img.createGraphics();
		g.setColor(Color.BLUE);
		g.fillRect(0, 0, 2, 2);
		g.dispose();
		File pngFile = new File(tempDir, "content.png");
		ImageIO.write(img, "png", pngFile);
		byte[] pngBytes = Files.readAllBytes(pngFile.toPath());
		AttachmentContent content = new AttachmentContent("testCustomer", "testModule", "testDocument", null, "testUser", "testId", "testAttr");
		content.attachment("content.png", MimeType.png, pngBytes);
		content.setContentId("0123456789abcdef0123456789abcdef");
		return content;
	}

	@Test
	void contentConstructorZeroSizeReturnsBytesDirectly() throws IOException {
		AttachmentContent content = createPngContent();
		byte[] expected = content.getContentBytes();
		Thumbnail t = new Thumbnail(content);
		assertNotNull(t.getBytes());
		assertArrayEquals(expected, t.getBytes());
	}

	@Test
	void contentConstructorZeroSizeReturnsMimeType() throws IOException {
		AttachmentContent content = createPngContent();
		Thumbnail t = new Thumbnail(content);
		assertEquals(MimeType.png, t.getMimeType());
	}

	@Test
	void contentConstructorWithWidthHeightZeroReturnsBytesDirectly() throws IOException {
		AttachmentContent content = createPngContent();
		byte[] expected = content.getContentBytes();
		Thumbnail t = new Thumbnail(content, 0, 0);
		assertNotNull(t.getBytes());
		assertArrayEquals(expected, t.getBytes());
	}

	@Test
	void contentConstructorWithDimensionsProducesCachedPngThumbnail() throws IOException {
		AttachmentContent content = createPngContent();

		Thumbnail first = new Thumbnail(content, 4, 4);
		Thumbnail second = new Thumbnail(content, 4, 4);

		assertNotNull(first.getBytes());
		assertEquals(MimeType.png, first.getMimeType());
		assertArrayEquals(first.getBytes(), second.getBytes());
	}

	@Test
	void contentConstructorProcessesMarkupPath() throws IOException {
		AttachmentContent content = createPngContent().markup("<svg><not-valid");

		Thumbnail t = new Thumbnail(content, 4, 4);

		assertNotNull(t.getBytes());
		assertEquals(MimeType.png, t.getMimeType());
	}

	// ---- InputStream constructor ----

	@Test
	void inputStreamConstructorWithSvgContent() throws IOException {
		// SVG is returned directly without resizing or file storage
		byte[] svgBytes = "<svg xmlns=\"http://www.w3.org/2000/svg\"><rect width=\"10\" height=\"10\"/></svg>".getBytes();
		Thumbnail t = new Thumbnail(new ByteArrayInputStream(svgBytes), "/test/image.svg", "image/svg+xml", 64, 64);
		assertNotNull(t.getBytes());
		assertEquals(MimeType.svg, t.getMimeType());
	}

	@Test
	void getMimeTypeReturnsNullForUnknownMimeType() throws IOException {
		// File constructor with unknown extension returns null mimeType
		File unknownFile = new File(tempDir, "test.unknownext");
		Files.write(unknownFile.toPath(), "data".getBytes());
		Thumbnail t = new Thumbnail(unknownFile);
		// MimeType.fromFileName returns null for unknown extensions
		assertNull(t.getMimeType());
	}

	// ---- Thumbnail(File, int, int) — exercises process() with file storage ----

	/** Creates a 10x10 red PNG file in tempDir. */
	private File createSmallPngFile() throws IOException {
		BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_INT_RGB);
		Graphics2D g = img.createGraphics();
		g.setColor(Color.RED);
		g.fillRect(0, 0, 10, 10);
		g.dispose();
		File png = new File(tempDir, "small.png");
		ImageIO.write(img, "png", png);
		return png;
	}

	@Test
	void fileWidthHeightConstructorProducesPngThumbnail() throws IOException {
		File pngFile = createSmallPngFile();
		Thumbnail t = new Thumbnail(pngFile, 4, 4);
		assertNotNull(t.getBytes());
		assertEquals(MimeType.png, t.getMimeType());
	}

	@Test
	void fileWidthHeightConstructorWritesCacheFile() throws IOException {
		File pngFile = createSmallPngFile();
		Thumbnail t = new Thumbnail(pngFile, 4, 4);
		// Verify a thumbnail was produced (cache dir is created inside THUMBNAIL_DIRECTORY)
		assertNotNull(t.getBytes());
		File thumbnailDir = new File(UtilImpl.THUMBNAIL_DIRECTORY);
		assertTrue(thumbnailDir.exists());
	}

	@Test
	void fileWidthHeightConstructorForSvgReturnsSvgDirectly() throws IOException {
		File svgFile = createSvgFile();
		Thumbnail t = new Thumbnail(svgFile, 64, 64);
		assertNotNull(t.getBytes());
		assertEquals(MimeType.svg, t.getMimeType());
	}

	@Disabled("ImageUtil.svg() uses CORE.getRepository() which is null in unit tests")
	@Test
	void fileWidthHeightConstructorForNonImageReturnsPlaceholderSvg() throws IOException {
		// A text file is not a valid image — process() should return a placeholder SVG
		File txtFile = new File(tempDir, "doc.docx");
		Files.write(txtFile.toPath(), "not an image".getBytes());
		Thumbnail t = new Thumbnail(txtFile, 64, 64);
		assertNotNull(t.getBytes());
		// Not an image → placeholder SVG byte array (ImageUtil.svg)
		assertEquals(MimeType.svg, t.getMimeType());
	}

	/** Writes a red PNG of the given size to tempDir. */
	private File createPngFile(String name, int width, int height) throws IOException {
		BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
		Graphics2D g = img.createGraphics();
		g.setColor(Color.RED);
		g.fillRect(0, 0, width, height);
		g.dispose();
		File png = new File(tempDir, name);
		ImageIO.write(img, "png", png);
		return png;
	}

	private static String size(Thumbnail thumbnail) throws IOException {
		BufferedImage image = ImageIO.read(new ByteArrayInputStream(thumbnail.getBytes()));
		return image.getWidth() + "x" + image.getHeight();
	}

	@Test
	void thumbnailShrinksToFitTheBoxKeepingItsShape() throws IOException {
		assertEquals("100x75", size(new Thumbnail(createPngFile("landscape.png", 400, 300), 100, 100)));
		assertEquals("75x100", size(new Thumbnail(createPngFile("portrait.png", 300, 400), 100, 100)));
		assertEquals("67x50", size(new Thumbnail(createPngFile("wide-box.png", 400, 300), 100, 50)));
	}

	@Test
	void thumbnailIsNotEnlargedPastTheImageSize() throws IOException {
		assertEquals("10x10", size(new Thumbnail(createSmallPngFile(), 1024, 1024)));
		assertEquals("40x30", size(new Thumbnail(createPngFile("fits-one-side.png", 40, 30), 40, 1024)));
	}

	private static long storedThumbnailCount() throws IOException {
		try (Stream<Path> files = Files.walk(new File(UtilImpl.THUMBNAIL_DIRECTORY).toPath())) {
			return files.filter(p -> p.toString().endsWith(".png")).count();
		}
	}

	private static File storedThumbnail(String name) throws IOException {
		try (Stream<Path> files = Files.walk(new File(UtilImpl.THUMBNAIL_DIRECTORY).toPath())) {
			return files.filter(p -> p.getFileName().toString().equals(name)).findFirst().orElseThrow().toFile();
		}
	}

	@Test
	void aThumbnailIsStoredUnderItsSizeWithNoTemporaryFileLeft() throws IOException {
		assertEquals("100x75", size(new Thumbnail(createPngFile("stored.png", 400, 300), 100, 100)));

		File stored = storedThumbnail("100x100.png");
		assertEquals("100x75", ImageIO.read(stored).getWidth() + "x" + ImageIO.read(stored).getHeight());
		String[] files = stored.getParentFile().list();
		assertNotNull(files);
		assertArrayEquals(new String[] {"100x100.png"}, files);
	}

	@Test
	void aThumbnailOfAFileChangedSinceItWasStoredIsMadeAgain() throws IOException {
		File pngFile = createPngFile("changing.png", 400, 300);
		assertEquals("100x75", size(new Thumbnail(pngFile, 100, 100)));

		// Redeployed with a different image of the same name
		BufferedImage tall = new BufferedImage(200, 400, BufferedImage.TYPE_INT_RGB);
		ImageIO.write(tall, "png", pngFile);
		assertTrue(pngFile.setLastModified(System.currentTimeMillis() + 10_000L));

		assertEquals("50x100", size(new Thumbnail(pngFile, 100, 100)));
		// and the new one is stored, over the old one
		assertEquals("50x100", size(new Thumbnail(pngFile, 100, 100)));
		assertEquals(1, storedThumbnailCount());
	}

	@Test
	void deleteStoredThumbnailsOlderThanDeletesOnlyOldThumbnailsAndEmptyFolders() throws IOException {
		Path root = new File(UtilImpl.THUMBNAIL_DIRECTORY).toPath();
		Path oldFolder = Files.createDirectories(root.resolve("aa/bb/old"));
		Path mixedFolder = Files.createDirectories(root.resolve("aa/cc/mixed"));
		Path emptyFolder = Files.createDirectories(root.resolve("dd/ee/empty"));
		Path oldFile = Files.createFile(oldFolder.resolve("128x128.png"));
		Path oldMarker = Files.createFile(mixedFolder.resolve("NOT_AN_IMAGE.txt"));
		Path newFile = Files.createFile(mixedFolder.resolve("256x256.png"));
		long old = System.currentTimeMillis() - java.util.concurrent.TimeUnit.DAYS.toMillis(91);
		for (Path path : new Path[] {oldFile, oldMarker, oldFolder, oldFolder.getParent(), root.resolve("aa"), mixedFolder, root.resolve("aa/cc")}) {
			Files.setLastModifiedTime(path, java.nio.file.attribute.FileTime.fromMillis(old));
		}

		assertEquals(2, Thumbnail.deleteStoredThumbnailsOlderThan(90));

		assertAll(() -> assertFalse(Files.exists(oldFolder.getParent()), "the old folders, emptied, are deleted"),
					() -> assertFalse(Files.exists(oldMarker)),
					() -> assertTrue(Files.exists(newFile), "a new thumb nail is kept, and so its folder"),
					() -> assertFalse(Files.exists(emptyFolder.getParent().getParent()), "an empty folder is deleted, however new"),
					() -> assertTrue(Files.exists(root)));
	}

	@Test
	void deleteStoredThumbnailsOlderThanDoesNothingWithoutAThumbnailDirectory() throws IOException {
		UtilImpl.THUMBNAIL_DIRECTORY = tempDir.getAbsolutePath() + "/missing/";
		assertEquals(0, Thumbnail.deleteStoredThumbnailsOlderThan(90));
	}

	@Test
	void deleteStoredThumbnailsOlderThanUsesTheContentDirectoryWhenNoThumbnailDirectoryIsSet() throws IOException {
		String savedContentDirectory = UtilImpl.CONTENT_DIRECTORY;
		try {
			UtilImpl.THUMBNAIL_DIRECTORY = null;
			UtilImpl.CONTENT_DIRECTORY = tempDir.getAbsolutePath() + "/content/";
			Path old = Files.createFile(Files.createDirectories(new File(Util.getThumbnnailDirectory(), "aa").toPath()).resolve("64x64.png"));
			Files.setLastModifiedTime(old, java.nio.file.attribute.FileTime.fromMillis(System.currentTimeMillis() - java.util.concurrent.TimeUnit.DAYS.toMillis(91)));

			assertEquals(1, Thumbnail.deleteStoredThumbnailsOlderThan(90));
		}
		finally {
			UtilImpl.CONTENT_DIRECTORY = savedContentDirectory;
		}
	}

	@Test
	void aThumbnailThatCannotBeStoredIsStillServed() throws IOException {
		File pngFile = createPngFile("unstorable.png", 400, 300);
		assertNotNull(new Thumbnail(pngFile, 100, 100).getBytes());
		File stored = storedThumbnail("100x100.png");
		File folder = stored.getParentFile();
		Files.delete(stored.toPath());
		assertTrue(folder.setWritable(false));
		try {
			Thumbnail thumbnail = new Thumbnail(pngFile, 100, 100);

			assertEquals(MimeType.png, thumbnail.getMimeType());
			assertEquals("100x75", size(thumbnail));
			assertEquals(0, storedThumbnailCount());
		}
		finally {
			assertTrue(folder.setWritable(true));
		}
	}

	@Test
	void decodeThreadsIsHalfTheProcessorsOrTheSettingButWithinAQuarterOfTheHeap() {
		long megabyte = 1024L * 1024L;
		assertAll(
			() -> assertEquals(4, Thumbnail.decodeThreads(0, 8, 4096 * megabyte), "half the processors"),
			() -> assertEquals(1, Thumbnail.decodeThreads(0, 1, 4096 * megabyte), "at least 1"),
			() -> assertEquals(3, Thumbnail.decodeThreads(3, 8, 4096 * megabyte), "the setting"),
			// A quarter of 4GB is room for eight 128MB thumbnails
			() -> assertEquals(8, Thumbnail.decodeThreads(10, 8, 4096 * megabyte), "the setting, limited by the heap"),
			() -> assertEquals(8, Thumbnail.decodeThreads(0, 32, 4096 * megabyte), "half the processors, limited by the heap"),
			() -> assertEquals(1, Thumbnail.decodeThreads(10, 8, 512 * megabyte), "a quarter of 512MB is room for one"),
			() -> assertEquals(1, Thumbnail.decodeThreads(10, 8, 100 * megabyte), "always at least 1"));
	}

	@Test
	void aFailedDecodeGivesBackItsDecodePermit() throws IOException {
		// A PNG cut off in its image data - its header reads, but decoding it fails
		BufferedImage noise = new BufferedImage(200, 200, BufferedImage.TYPE_INT_RGB);
		java.util.Random random = new java.util.Random(4);
		for (int x = 0; x < 200; x++) {
			for (int y = 0; y < 200; y++) {
				noise.setRGB(x, y, random.nextInt());
			}
		}
		java.io.ByteArrayOutputStream png = new java.io.ByteArrayOutputStream();
		ImageIO.write(noise, "png", png);
		byte[] truncated = java.util.Arrays.copyOf(png.toByteArray(), png.size() / 2);
		File good = createPngFile("good.png", 400, 300);
		UtilImpl.THUMBNAIL_FILE_STORAGE = false;

		// More failures than there can be permits - a leaked permit would make the last thumbnail wait 60 seconds
		assertTimeoutPreemptively(java.time.Duration.ofSeconds(20), () -> {
			for (int i = 0; i < 100; i++) {
				assertThrows(IOException.class,
								() -> new Thumbnail(new ByteArrayInputStream(truncated), "/broken.png", "image/png", 64, 64),
								"failure " + i);
			}
			assertEquals("64x48", size(new Thumbnail(good, 64, 64)));
		});
	}

	@Test
	void deleteStoredThumbnailsRemovesAnAttachmentsThumbnails() throws IOException {
		AttachmentContent content = createPngContent();
		assertNotNull(new Thumbnail(content, 4, 4).getBytes());
		assertTrue(storedThumbnailCount() > 0);

		// The content ID is matched whatever its case, as the folder name is lower case
		Thumbnail.deleteStoredThumbnails("0123456789ABCDEF0123456789ABCDEF");
		assertEquals(0, storedThumbnailCount());
		// Deleting again, or deleting thumbnails that were never made, is harmless
		Thumbnail.deleteStoredThumbnails("0123456789abcdef0123456789abcdef");
		Thumbnail.deleteStoredThumbnails("ffffffffffffffffffffffffffffffff");
	}

	@Test
	void fileWidthHeightConstructorSecondCallReturnsCachedPng() throws IOException {
		File pngFile = createSmallPngFile();
		// First call creates the cache file
		Thumbnail first = new Thumbnail(pngFile, 4, 4);
		// Second call should load from cache
		Thumbnail second = new Thumbnail(pngFile, 4, 4);
		assertNotNull(second.getBytes());
		assertEquals(MimeType.png, second.getMimeType());
		assertArrayEquals(first.getBytes(), second.getBytes());
	}

	// The waits take a deadline, so one already passed times out at once

	@Test
	void acquireTimesOutWhileAnotherThreadHoldsTheImageLock() throws InterruptedException {
		ReentrantLock lock = new ReentrantLock(true);
		// A lock is kept by a thread that ends without unlocking it
		Thread holder = new Thread(lock::lock);
		holder.start();
		holder.join();

		long deadline = System.nanoTime();
		assertThrows(Thumbnail.BusyException.class, () -> Thumbnail.acquire(lock, deadline));
	}

	@Test
	void acquireDecoderTimesOutWhileEveryPermitIsTaken() {
		int taken = Thumbnail.DECODERS.drainPermits();
		try {
			long deadline = System.nanoTime();
			assertThrows(Thumbnail.BusyException.class, () -> Thumbnail.acquireDecoder(deadline));
		}
		finally {
			Thumbnail.DECODERS.release(taken);
		}
	}

	@Test
	void acquireWhenInterruptedThrowsAndKeepsTheInterrupt() {
		ReentrantLock lock = new ReentrantLock(true);
		long deadline = System.nanoTime() + TimeUnit.MINUTES.toNanos(1);
		Thread.currentThread().interrupt();
		try {
			assertThrowsExactly(InterruptedIOException.class, () -> Thumbnail.acquire(lock, deadline));
			assertFalse(lock.isLocked());
		}
		finally {
			assertTrue(Thread.interrupted(), "the interrupt is kept");
		}
	}

	@Test
	void acquireDecoderWhenInterruptedThrowsAndKeepsTheInterrupt() {
		int permits = Thumbnail.DECODERS.availablePermits();
		long deadline = System.nanoTime() + TimeUnit.MINUTES.toNanos(1);
		Thread.currentThread().interrupt();
		try {
			assertThrowsExactly(InterruptedIOException.class, () -> Thumbnail.acquireDecoder(deadline));
			assertEquals(permits, Thumbnail.DECODERS.availablePermits());
		}
		finally {
			assertTrue(Thread.interrupted(), "the interrupt is kept");
		}
	}
}
