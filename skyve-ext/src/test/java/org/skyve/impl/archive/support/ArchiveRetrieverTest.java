package org.skyve.impl.archive.support;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;

@SuppressWarnings({"static-method", "boxing"})
class ArchiveRetrieverTest {
	private static final String ORIGINAL_CONTENT_DIRECTORY = UtilImpl.CONTENT_DIRECTORY;

	@TempDir
	Path tempDir;

	@AfterEach
	void restoreContentDirectory() {
		UtilImpl.CONTENT_DIRECTORY = ORIGINAL_CONTENT_DIRECTORY;
	}

	@Test
	void archiveEntryCacheKeyIncludesDocumentTypeFileOffsetAndLength() throws Exception {
		Object entry = newArchiveEntry(new ArchiveDocConfig("sales", "Order", "orders", 30), "orders.archive", 12L, 34L);

		Method method = entry.getClass().getDeclaredMethod("cacheKey");
		method.setAccessible(true);

		assertEquals("sales-Order-orders.archive-12-34", method.invoke(entry));
	}

	@Test
	void readLineReturnsRequestedBytesUsingArchiveCharset() throws Exception {
		Path archiveFile = tempDir.resolve("sample.archive");
		Files.writeString(archiveFile, "alpha\nbeta\ngamma", ArchiveUtils.ARCHIVE_CHARSET);
		Method method = ArchiveRetriever.class.getDeclaredMethod("readLine", Path.class, long.class, long.class);
		method.setAccessible(true);

		assertEquals("beta", method.invoke(ArchiveRetriever.getInstance(), archiveFile, 6L, 4L));
	}

	@Test
	void readLineRejectsLengthsThatCannotFitByteArray() throws Exception {
		Path archiveFile = tempDir.resolve("sample.archive");
		Files.writeString(archiveFile, "alpha", ArchiveUtils.ARCHIVE_CHARSET);
		Method method = ArchiveRetriever.class.getDeclaredMethod("readLine", Path.class, long.class, long.class);
		method.setAccessible(true);

		assertThrows(ArithmeticException.class,
				() -> invokeReadLine(method, archiveFile, 0L, ((long) Integer.MAX_VALUE) + 1));
	}

	@Test
	void getArchiveFilePathReturnsExistingConfiguredArchiveFile() throws Exception {
		UtilImpl.CONTENT_DIRECTORY = tempDir.toString();
		ArchiveDocConfig docConfig = new ArchiveDocConfig("sales", "Order", "orders", 30);
		Files.createDirectories(docConfig.getArchiveDirectory());
		Path archiveFile = docConfig.getArchiveDirectory()
									.resolve("orders.archive");
		Files.writeString(archiveFile, "{}", ArchiveUtils.ARCHIVE_CHARSET);
		Object entry = newArchiveEntry(docConfig, archiveFile.getFileName().toString(), 0L, 2L);
		Method method = ArchiveRetriever.class.getDeclaredMethod("getArchiveFilePath", entry.getClass());
		method.setAccessible(true);

		assertEquals(archiveFile, method.invoke(ArchiveRetriever.getInstance(), entry));
	}

	@Test
	void getArchiveFilePathRejectsMissingArchiveFile() throws Exception {
		UtilImpl.CONTENT_DIRECTORY = tempDir.toString();
		ArchiveDocConfig docConfig = new ArchiveDocConfig("sales", "Order", "orders", 30);
		Object entry = newArchiveEntry(docConfig, "missing.archive", 0L, 2L);
		Method method = ArchiveRetriever.class.getDeclaredMethod("getArchiveFilePath", entry.getClass());
		method.setAccessible(true);

		assertThrows(IllegalArgumentException.class,
				() -> invokeGetArchiveFilePath(method, entry));
	}

	private static Object newArchiveEntry(ArchiveDocConfig docConfig, String fileName, long offset, long length)
	throws Exception {
		Class<?> clazz = Class.forName("org.skyve.impl.archive.support.ArchiveRetriever$ArchiveEntry");
		Constructor<?> constructor = clazz.getDeclaredConstructor(ArchiveDocConfig.class, String.class, long.class, long.class);
		constructor.setAccessible(true);
		return constructor.newInstance(docConfig, fileName, Long.valueOf(offset), Long.valueOf(length));
	}

	private static Object invokeReadLine(Method method, Path file, long offset, long length) throws Throwable {
		try {
			return method.invoke(ArchiveRetriever.getInstance(), file, Long.valueOf(offset), Long.valueOf(length));
		}
		catch (ReflectiveOperationException e) {
			throw e.getCause();
		}
	}

	private static Object invokeGetArchiveFilePath(Method method, Object entry) throws Throwable {
		try {
			return method.invoke(ArchiveRetriever.getInstance(), entry);
		}
		catch (ReflectiveOperationException e) {
			throw e.getCause();
		}
	}
}
