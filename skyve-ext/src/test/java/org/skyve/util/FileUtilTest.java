package org.skyve.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.JarInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.content.MimeType;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.controller.Download;

@SuppressWarnings("static-method")
class FileUtilTest {
	@TempDir
	Path tempDir;

	@Test
	void constructorCreatesInstance() {
		assertNotNull(new FileUtil());
	}

	@Test
	void bytesAndStringReadFileContents() throws Exception {
		Path file = tempDir.resolve("sample.txt");
		Files.writeString(file, "hello\nworld", StandardCharsets.UTF_8);

		assertArrayEquals("hello\nworld".getBytes(StandardCharsets.UTF_8), FileUtil.bytes(file.toFile()));
		assertArrayEquals("stream".getBytes(StandardCharsets.UTF_8),
				FileUtil.bytes(new ByteArrayInputStream("stream".getBytes(StandardCharsets.UTF_8))));
		assertEquals("hello\nworld\n", FileUtil.string(file.toFile()));
	}

	@Test
	void safeFileNameEncodesIllegalCharactersAndTruncates() {
		String safeName = FileUtil.safeFileName("bad:file?.txt");
		String truncated = FileUtil.safeFileName("x".repeat(200));

		assertEquals("bad%3Afile%3F%2Etxt", safeName);
		assertEquals(127, truncated.length());
	}

	@Test
	void constructSafeFilePathCreatesDirectoriesAndNormalisesExtension() {
		String path = FileUtil.constructSafeFilePath(tempDir.toString(), "bad:file", "txt", true, "folder:name");
		File file = new File(path);

		assertTrue(file.getParentFile().isDirectory());
		assertThat(file.getName(), is("bad%3Afile.txt"));
		assertThat(file.getParentFile().getName(), is("folder%3Aname"));
	}

	@Test
	void prepareZipDownloadCreatesZipBytesForDirectory() throws Exception {
		Path directory = Files.createDirectory(tempDir.resolve("zipSource"));
		Files.writeString(directory.resolve("alpha.txt"), "alpha", StandardCharsets.UTF_8);
		Files.createDirectories(directory.resolve("nested"));
		Files.writeString(directory.resolve("nested/bravo.txt"), "bravo", StandardCharsets.UTF_8);

		Download download = FileUtil.prepareZipDownload(directory.toString(), "archive.zip");

		assertEquals("archive.zip", download.getFileName());
		assertEquals(MimeType.zip, download.getMimeType());
		assertNotNull(download.getBytes());

		List<String> names = new ArrayList<>();
		try (ZipInputStream input = new ZipInputStream(new ByteArrayInputStream(download.getBytes()))) {
			ZipEntry entry;
			while ((entry = input.getNextEntry()) != null) {
				names.add(entry.getName());
			}
		}

		assertTrue(names.contains("alpha.txt"));
		assertTrue(names.contains("nested/bravo.txt"));
	}

	@Test
	void createZipArchiveWritesZipFile() throws Exception {
		Path directory = Files.createDirectory(tempDir.resolve("zipFileSource"));
		Files.writeString(directory.resolve("alpha.txt"), "alpha", StandardCharsets.UTF_8);
		File zipFile = tempDir.resolve("archive.zip").toFile();

		FileUtil.createZipArchive(directory.toFile(), zipFile);

		assertTrue(zipFile.isFile());
		try (ZipInputStream input = new ZipInputStream(Files.newInputStream(zipFile.toPath()))) {
			ZipEntry entry = input.getNextEntry();
			assertNotNull(entry);
			assertEquals("alpha.txt", entry.getName());
		}
	}

	@Test
	void createJarArchiveIncludesManifestAndFiles() throws Exception {
		Path directory = Files.createDirectory(tempDir.resolve("jarSource"));
		Files.writeString(directory.resolve("alpha.txt"), "alpha", StandardCharsets.UTF_8);
		// Subdirectory triggers the recursive getAllFiles() branch
		Path sub = Files.createDirectory(directory.resolve("nested"));
		Files.writeString(sub.resolve("bravo.txt"), "bravo", StandardCharsets.UTF_8);

		ByteArrayOutputStream output = new ByteArrayOutputStream();
		FileUtil.createJarArchive(directory.toFile(), output);

		List<String> names = new ArrayList<>();
		try (JarInputStream input = new JarInputStream(new ByteArrayInputStream(output.toByteArray()))) {
			assertNotNull(input.getManifest());
			ZipEntry entry;
			while ((entry = input.getNextEntry()) != null) {
				names.add(entry.getName());
			}
		}

		assertTrue(names.contains("alpha.txt"));
		assertTrue(names.contains("nested/bravo.txt"));
	}

	@Test
	void extractZipArchiveRejectsPathTraversalEntry() throws Exception {
		// Build a zip with a path-traversal entry name (Zip Slip attack vector).
		// "../../evil.txt" has a directory component so mkdirs() catches it first.
		Path zipPath = tempDir.resolve("evil.zip");
		try (ZipOutputStream output = new ZipOutputStream(new FileOutputStream(zipPath.toFile()))) {
			output.putNextEntry(new ZipEntry("../../evil.txt"));
			output.write("pwned".getBytes(StandardCharsets.UTF_8));
			output.closeEntry();
		}

		Path outputDir = tempDir.resolve("unzippedEvil");
		assertThrows(IOException.class, () -> FileUtil.extractZipArchive(zipPath.toFile(), outputDir.toFile()));
	}

	@Test
	void extractZipArchiveRejectsPathTraversalEntryWithNoDirectoryPart() throws Exception {
		// Entry name ".." has no slash so dirpart() returns null and mkdirs() is
		// never called — exercising the guard inside extractFile() directly.
		Path zipPath = tempDir.resolve("evil2.zip");
		try (ZipOutputStream output = new ZipOutputStream(new FileOutputStream(zipPath.toFile()))) {
			output.putNextEntry(new ZipEntry(".."));
			output.write("pwned".getBytes(StandardCharsets.UTF_8));
			output.closeEntry();
		}

		Path outputDir = tempDir.resolve("unzippedEvil2");
		assertThrows(IOException.class, () -> FileUtil.extractZipArchive(zipPath.toFile(), outputDir.toFile()));
	}

	@Test
	void extractZipArchiveCreatesParentDirectoriesForFileEntries() throws Exception {
		Path zipPath = tempDir.resolve("archive.zip");
		try (ZipOutputStream output = new ZipOutputStream(new FileOutputStream(zipPath.toFile()))) {
			output.putNextEntry(new ZipEntry("nested/child.txt"));
			output.write("child".getBytes(StandardCharsets.UTF_8));
			output.closeEntry();
			output.putNextEntry(new ZipEntry("nested/"));
			output.closeEntry();
		}

		Path outputDir = tempDir.resolve("unzipped");
		FileUtil.extractZipArchive(zipPath.toFile(), outputDir.toFile());

		assertEquals("child", Files.readString(outputDir.resolve("nested/child.txt"), StandardCharsets.UTF_8));
	}

	@Test
	void deleteAndCopyHandlePlainFilesAndDirectories() throws Exception {
		Path source = tempDir.resolve("source.txt");
		Path copied = tempDir.resolve("copied.txt");
		Files.writeString(source, "copy-me", StandardCharsets.UTF_8);

		FileUtil.copy(source.toFile(), copied.toFile());
		assertEquals("copy-me", Files.readString(copied, StandardCharsets.UTF_8));

		Path directory = Files.createDirectory(tempDir.resolve("deleteRoot"));
		Files.writeString(directory.resolve("child.txt"), "bye", StandardCharsets.UTF_8);
		Files.createDirectories(directory.resolve("nested"));
		Files.writeString(directory.resolve("nested/grandchild.txt"), "bye", StandardCharsets.UTF_8);

		FileUtil.delete(directory.toFile());
		assertFalse(Files.exists(directory));
	}

	@Test
	void listFilesFiltersAndSortsByName() throws IOException {
		Path directory = Files.createDirectory(tempDir.resolve("listFiles"));
		Files.writeString(directory.resolve("bravo.txt"), "b", StandardCharsets.UTF_8);
		Files.writeString(directory.resolve("Alpha.txt"), "a", StandardCharsets.UTF_8);
		Files.writeString(directory.resolve("charlie.csv"), "c", StandardCharsets.UTF_8);

		File[] ascending = FileUtil.listFiles(directory.toFile(), ".*\\.txt", SortDirection.ascending);
		File[] descending = FileUtil.listFiles(directory.toFile(), ".*\\.txt", SortDirection.descending);
		File[] unsorted = FileUtil.listFiles(directory.toFile(), null, null);

		assertNotNull(ascending);
		assertNotNull(descending);
		assertNotNull(unsorted);
		assertEquals(2, ascending.length);
		assertEquals("Alpha.txt", ascending[0].getName());
		assertEquals("bravo.txt", ascending[1].getName());
		assertEquals("bravo.txt", descending[0].getName());
		assertEquals("Alpha.txt", descending[1].getName());
		assertEquals(3, unsorted.length);
	}
}