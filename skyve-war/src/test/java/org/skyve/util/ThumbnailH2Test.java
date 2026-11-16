package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.content.MimeType;
import org.skyve.impl.util.UtilImpl;

import util.AbstractH2Test;

/**
 * Thumbnails of originals that aren't images, which need the repository for their file type icons.
 */
class ThumbnailH2Test extends AbstractH2Test {
	@TempDir
	File tempDir;

	private String savedThumbnailDirectory;
	private boolean savedThumbnailFileStorage;

	@BeforeEach
	void storeThumbnailsInTheTempDir() {
		savedThumbnailDirectory = UtilImpl.THUMBNAIL_DIRECTORY;
		savedThumbnailFileStorage = UtilImpl.THUMBNAIL_FILE_STORAGE;
		UtilImpl.THUMBNAIL_DIRECTORY = tempDir.getAbsolutePath() + "/thumbnails/";
		UtilImpl.THUMBNAIL_FILE_STORAGE = true;
	}

	@AfterEach
	void restoreThumbnailSettings() {
		UtilImpl.THUMBNAIL_DIRECTORY = savedThumbnailDirectory;
		UtilImpl.THUMBNAIL_FILE_STORAGE = savedThumbnailFileStorage;
	}

	@Test
	@SuppressWarnings("static-method")
	void aNonImageGetsItsFileTypeIconAndIsRememberedAsNotAnImage() throws IOException {
		Thumbnail first = new Thumbnail(new ByteArrayInputStream("not an image".getBytes(StandardCharsets.UTF_8)),
											"/notes.txt",
											"text/plain",
											64,
											64);
		assertAll(() -> assertEquals(MimeType.svg, first.getMimeType()),
					() -> assertTrue(new String(first.getBytes(), StandardCharsets.UTF_8).startsWith("<svg width=\"48px\" height=\"64px\""),
										"the 72 x 96 txt file type icon, fitted to the box"),
					() -> assertEquals(1, notAnImageMarkerCount()));

		// The marker is served without reading the original again
		try (InputStream unread = new InputStream() {
			@Override
			public int read() throws IOException {
				throw new IOException("The original was read again");
			}
		}) {
			Thumbnail second = new Thumbnail(unread, "/notes.txt", "text/plain", 64, 64);
			assertAll(() -> assertEquals(MimeType.svg, second.getMimeType()),
						() -> assertEquals(new String(first.getBytes(), StandardCharsets.UTF_8),
											new String(second.getBytes(), StandardCharsets.UTF_8)));
		}
	}

	private static long notAnImageMarkerCount() throws IOException {
		try (Stream<Path> paths = Files.walk(new File(UtilImpl.THUMBNAIL_DIRECTORY).toPath())) {
			return paths.filter(p -> "NOT_AN_IMAGE.txt".equals(p.getFileName().toString())).count();
		}
	}
}
