package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.withSettings;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import javax.imageio.ImageIO;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;

import jakarta.annotation.Nonnull;

/**
 * Tests the photos and files of {@link SailTestFiles} and the reuse of every file, without a browser. The video is recorded and
 * played in {@code SailTestFilesIT}. Each test deletes the files it checks first, so it proves they are made, not
 * reused from an earlier run.
 */
class SailTestFilesTest {
	/** A modification time no file made now can have, to show that a reused file wasn't written again. */
	private static final FileTime LONG_AGO = FileTime.fromMillis(86_400_000L);

	@Test
	@SuppressWarnings("static-method")
	void aPhotoOfAByteSizeHasExactlyThatSizeAndDecodes() throws IOException {
		// under the noise sample, noise with one comment, near the 64 KB segment edge, the 12 megapixel cap, and either side of 10 MB
		List<Long> sizes = List.of(Long.valueOf(2_000L), Long.valueOf(50_000L), Long.valueOf(4L * 1024 * 1024),
									Long.valueOf(4L * 1024 * 1024 + 1), Long.valueOf(10L * 1024 * 1024), Long.valueOf(10L * 1024 * 1024 + 1),
									Long.valueOf(20L * 1024 * 1024 + 3));
		for (Long size : sizes) {
			Files.deleteIfExists(SailTestFiles.FOLDER.resolve("photo-" + size + ".jpg"));
		}
		assertAll(sizes.stream().map(size -> (Executable) () -> {
			Path photo = SailTestFiles.photo(size.longValue());
			BufferedImage image = ImageIO.read(photo.toFile());
			assertAll(() -> assertEquals(SailTestFiles.FOLDER.resolve("photo-" + size + ".jpg"), photo, size + ": the path"),
						() -> assertEquals(size.longValue(), Files.size(photo), size + ": the size"),
						() -> assertNotNull(image, size + ": decodes"),
						() -> assertTrue((image == null) || (image.getWidth() > 1), size + ": noise, not a dot"));
		}));
	}

	@Test
	@SuppressWarnings("static-method")
	void paddingFillsEveryByteWithCommentSegments() {
		byte[] jpeg = {(byte) 0xFF, (byte) 0xD8, (byte) 0xFF, (byte) 0xE0, 0, 4, 1, 2, (byte) 0xFF, (byte) 0xD9};
		// one short segment, a full one, and a full one whose remainder (1 to 3 bytes) is too small for a segment of its own
		List<Integer> paddings = List.of(Integer.valueOf(4), Integer.valueOf(2 + 65_535), Integer.valueOf(2 + 65_535 + 1),
											Integer.valueOf(2 + 65_535 + 3), Integer.valueOf(3 * 65_537 + 10));
		assertAll(Stream.concat(Stream.<Executable> of(() -> assertSame(jpeg, SailTestFiles.pad(jpeg, 0), "no padding: the same bytes")),
								paddings.stream().map(padding -> (Executable) () -> {
									byte[] padded = SailTestFiles.pad(jpeg, padding.intValue());
									assertAll(() -> assertEquals(jpeg.length + padding.intValue(), padded.length, padding + ": the size"),
												() -> assertArrayEquals(new byte[] {(byte) 0xFF, (byte) 0xD8, (byte) 0xFF, (byte) 0xE0, 0, 4, 1, 2},
																		java.util.Arrays.copyOf(padded, 8), padding + ": the JFIF segment stays first"),
												() -> assertEquals(padding.intValue(), comments(padded, 8), padding + ": comment segments fill the padding"),
												() -> assertArrayEquals(new byte[] {(byte) 0xFF, (byte) 0xD9},
																		java.util.Arrays.copyOfRange(padded, padded.length - 2, padded.length), padding + ": the rest follows"));
								})));
	}

	/** Walks the comment segments from an offset, checking each one's marker, and gives the bytes they cover. */
	private static int comments(@Nonnull byte[] jpeg, int from) {
		int at = from;
		while (((jpeg[at] & 0xFF) == 0xFF) && ((jpeg[at + 1] & 0xFF) == 0xFE)) {
			at += 2 + (((jpeg[at + 2] & 0xFF) << 8) | (jpeg[at + 3] & 0xFF));
		}
		return at - from;
	}

	@Test
	@SuppressWarnings("static-method")
	void aPhotoHasTheDimensionsAsked() throws IOException {
		List<int[]> dimensions = List.of(new int[] {800, 600}, new int[] {1, 1}, new int[] {3, 2000}, new int[] {4000, 3000});
		for (int[] d : dimensions) {
			Files.deleteIfExists(SailTestFiles.FOLDER.resolve("photo-" + d[0] + "x" + d[1] + ".jpg"));
		}
		assertAll(dimensions.stream().map(d -> (Executable) () -> {
			String name = d[0] + "x" + d[1];
			Path photo = SailTestFiles.photo(d[0], d[1]);
			BufferedImage image = ImageIO.read(photo.toFile());
			assertAll(() -> assertEquals(SailTestFiles.FOLDER.resolve("photo-" + name + ".jpg"), photo, name + ": the path"),
						() -> assertNotNull(image, name + ": decodes"),
						() -> assertEquals(d[0], (image == null) ? -1 : image.getWidth(), name + ": the width"),
						() -> assertEquals(d[1], (image == null) ? -1 : image.getHeight(), name + ": the height"));
		}));
		assertTrue(Files.size(SailTestFiles.photo(800, 600)) < 100_000, "a gradient is small");
	}

	@Test
	@SuppressWarnings("static-method")
	void theOversizePhotoIsTooLargeInPixelsAndBytes() throws IOException {
		Path expected = SailTestFiles.FOLDER.resolve("photo-oversize-9000x7000.jpg");
		Files.deleteIfExists(expected);
		Path photo = SailTestFiles.oversizePhoto();
		// read from the header: decoding 63 megapixels would take seconds and 250 MB
		int[] dimensions = SailTestFiles.dimensions(photo);
		assertAll(() -> assertEquals(expected, photo, "the path"),
					() -> assertTrue(Files.size(photo) >= SailTestFiles.OVERSIZE_MINIMUM_BYTES, "over 10 MB: " + Files.size(photo)),
					() -> assertArrayEquals(new int[] {9000, 7000}, dimensions, "the dimensions"));
	}

	@Test
	@SuppressWarnings("static-method")
	void aFileThatIsThereWithTheRightSizeIsReusedNotWritten() throws IOException {
		Path bySize = SailTestFiles.photo(50_000L);
		Path byDimensions = SailTestFiles.photo(800, 600);
		Path oversize = SailTestFiles.oversizePhoto();
		Path video = SailTestFiles.FOLDER.resolve("video-1000.webm");
		Files.write(video, new byte[1000]);
		List<Path> files = List.of(bySize, byDimensions, oversize, video);
		for (Path file : files) {
			Files.setLastModifiedTime(file, LONG_AGO);
		}
		Object[] keys = {key(bySize), key(byDimensions), key(oversize), key(video)};
		WebDriver driver = mock(WebDriver.class);
		List<Path> again = List.of(SailTestFiles.photo(50_000L), SailTestFiles.photo(800, 600), SailTestFiles.oversizePhoto(),
									SailTestFiles.video(driver, 1000L), SailTestFiles.video(driver, 1000L, java.time.Duration.ZERO));
		assertAll(Stream.concat(Stream.<Executable> of(() -> verifyNoInteractions(driver)),
								IntStream.range(0, again.size()).mapToObj(i -> (Executable) () -> {
									int f = Math.min(i, files.size() - 1); // both video calls are of the one file
									Path file = files.get(f);
									assertAll(() -> assertEquals(file, again.get(i), file.getFileName() + ": the same path"),
												() -> assertEquals(LONG_AGO, Files.getLastModifiedTime(file), file.getFileName() + ": not written again"),
												() -> assertEquals(keys[f], key(file), file.getFileName() + ": the same file"));
								})));
	}

	/** Gives the file's identity (its inode, where the file system has one), or its creation time where not. */
	private static Object key(@Nonnull Path file) throws IOException {
		Object result = Files.readAttributes(file, java.nio.file.attribute.BasicFileAttributes.class).fileKey();
		return (result == null) ? Files.readAttributes(file, java.nio.file.attribute.BasicFileAttributes.class).creationTime() : result;
	}

	@Test
	@SuppressWarnings("static-method")
	void aFileThatIsThereWithTheWrongSizeIsMadeAgain() throws IOException {
		Files.createDirectories(SailTestFiles.FOLDER);
		Path bySize = SailTestFiles.FOLDER.resolve("photo-50000.jpg");
		Files.writeString(bySize, "cut short", StandardCharsets.US_ASCII);
		// a real JPEG, but 10 x 10, under the name of an 800 x 600 one
		Path byDimensions = SailTestFiles.FOLDER.resolve("photo-800x600.jpg");
		Files.copy(SailTestFiles.photo(10, 10), byDimensions, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
		Path video = SailTestFiles.FOLDER.resolve("video-1000.webm");
		Files.write(video, new byte[999]);
		// a browser that records nothing: its script gives null
		WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class).defaultAnswer(RETURNS_DEEP_STUBS));
		assertAll(() -> assertEquals(50_000L, Files.size(SailTestFiles.photo(50_000L)), "the photo by size"),
					() -> assertArrayEquals(new int[] {800, 600}, SailTestFiles.dimensions(SailTestFiles.photo(800, 600)), "the photo by dimensions"),
					// a video one byte short goes to the browser again, which records nothing here, so it fails, leaving the file as it was
					() -> assertThrows(IllegalStateException.class, () -> SailTestFiles.video(driver, 1000L), "the video"),
					() -> verify((JavascriptExecutor) driver).executeScript(anyString(), any(), any()),
					() -> assertEquals(999L, Files.size(video), "the short video is left"),
					() -> assertTrue(noPartials(), "no temporary files are left"));
	}

	private static boolean noPartials() throws IOException {
		try (Stream<Path> files = Files.list(SailTestFiles.FOLDER)) {
			return files.noneMatch(f -> f.getFileName().toString().endsWith(".partial"));
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void aSizeOrDimensionThatCantBeMadeIsRefused() {
		WebDriver driver = mock(WebDriver.class);
		assertAll(() -> assertThrows(IllegalArgumentException.class, () -> SailTestFiles.photo(0L), "0 bytes"),
					() -> assertThrows(IllegalArgumentException.class, () -> SailTestFiles.photo(-1L), "-1 bytes"),
					() -> assertThrows(IllegalArgumentException.class, () -> SailTestFiles.photo(100L), "100 bytes: smaller than any JPEG"),
					() -> assertThrows(IllegalArgumentException.class, () -> SailTestFiles.photo(3L * 1024 * 1024 * 1024), "3 GB"),
					() -> assertThrows(IllegalArgumentException.class, () -> SailTestFiles.photo(0, 10), "0 wide"),
					() -> assertThrows(IllegalArgumentException.class, () -> SailTestFiles.photo(10, 0), "0 high"),
					() -> assertThrows(IllegalArgumentException.class, () -> SailTestFiles.photo(65_501, 10), "too wide for a JPEG"),
					() -> assertThrows(IllegalArgumentException.class, () -> SailTestFiles.video(driver, 0L), "a video of 0 bytes"),
					() -> verifyNoInteractions(driver));
	}

	@Test
	@SuppressWarnings("static-method")
	void aFileHasExactlyTheSizeAndIsAllZeros() {
		// empty, one byte, a piece either side, and 12 MB (over the 10 MB upload limit)
		List<Long> sizes = List.of(Long.valueOf(0L), Long.valueOf(1L), Long.valueOf(65_535L), Long.valueOf(65_536L),
									Long.valueOf(65_537L), Long.valueOf(12L * 1024 * 1024));
		assertAll(sizes.stream().map(size -> (Executable) () -> {
			String name = "test-zeros-" + size + ".bin";
			Files.deleteIfExists(SailTestFiles.FOLDER.resolve(name));
			Path file = SailTestFiles.file(name, size.longValue());
			assertAll(() -> assertEquals(SailTestFiles.FOLDER.resolve(name), file, size + ": the path"),
						() -> assertTrue(file.isAbsolute(), size + ": absolute"),
						() -> assertEquals(size.longValue(), Files.size(file), size + ": the size"),
						() -> assertTrue(allZeros(file), size + ": all zeros"),
						() -> assertTrue(noPartials(), size + ": no temporary files are left"));
		}));
	}

	private static boolean allZeros(@Nonnull Path file) throws IOException {
		try (java.io.InputStream in = Files.newInputStream(file)) {
			byte[] buffer = new byte[8192];
			for (int read = in.read(buffer); read >= 0; read = in.read(buffer)) {
				for (int i = 0; i < read; i++) {
					if (buffer[i] != 0) {
						return false;
					}
				}
			}
		}
		return true;
	}

	@Test
	@SuppressWarnings("static-method")
	void aFileThatIsThereWithTheRightSizeIsReusedAndOneWithTheWrongSizeIsReplaced() throws IOException {
		Path file = SailTestFiles.file("test-reuse.bin", 1000L);
		Files.setLastModifiedTime(file, LONG_AGO);
		Object key = key(file);
		Path again = SailTestFiles.file("test-reuse.bin", 1000L);
		assertAll(() -> assertEquals(file, again, "the same path"),
					() -> assertEquals(LONG_AGO, Files.getLastModifiedTime(again), "not written again"),
					() -> assertEquals(key, key(again), "the same file"));
		// the same name at another size, and a file that is cut short, are made again
		assertAll(() -> assertEquals(2000L, Files.size(SailTestFiles.file("test-reuse.bin", 2000L)), "a larger size"),
					() -> assertTrue(allZeros(file), "still zeros"));
		Files.writeString(file, "cut short", StandardCharsets.US_ASCII);
		assertAll(() -> assertEquals(5000L, Files.size(SailTestFiles.file("test-reuse.bin", 5000L)), "a cut short file"),
					() -> assertTrue(allZeros(file), "zeros, not the old text"),
					() -> assertTrue(noPartials(), "no temporary files are left"));
	}

	@Test
	@SuppressWarnings("static-method")
	void aNameThatIsNotAPlainFileNameOrANegativeSizeIsRefused() {
		assertAll(Stream.of("", ".", "..", "a/b.bin", "../a.bin", "/a.bin", "a\\b.bin", "a.bin.partial", "photo-1.bin", "video-1.bin", "a\0b")
							.map(name -> (Executable) () -> assertThrows(IllegalArgumentException.class, () -> SailTestFiles.file(name, 1L),
																			"the name '" + name + "'")));
		assertAll(() -> assertThrows(IllegalArgumentException.class, () -> SailTestFiles.file("test-negative.bin", -1L), "-1 bytes"),
					() -> assertTrue(Files.notExists(SailTestFiles.FOLDER.resolve("test-negative.bin")), "nothing is written"));
	}
}
