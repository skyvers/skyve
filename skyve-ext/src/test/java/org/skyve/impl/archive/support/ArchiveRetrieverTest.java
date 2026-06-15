package org.skyve.impl.archive.support;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.StoredField;
import org.apache.lucene.document.StringField;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.store.ByteBuffersDirectory;
import org.apache.lucene.store.Directory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.domain.Bean;
import org.skyve.impl.archive.job.IndexArchivesJob;
import org.skyve.impl.archive.list.LuceneFilter;
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

	@Test
	void searchIndexReturnsEmptyListWhenNoDocumentsMatch() throws Exception {
		ArchiveDocConfig docConfig = new ArchiveDocConfig("sales", "Order", "orders", 30);
		LuceneFilter filter = new LuceneFilter();
		filter.addEquals(Bean.DOCUMENT_ID, "missing");
		Method method = ArchiveRetriever.class.getDeclaredMethod("searchIndex", ArchiveDocConfig.class, LuceneFilter.class, int.class);
		method.setAccessible(true);

		try (Directory directory = new ByteBuffersDirectory()) {
			writeArchiveIndex(directory, archiveDocument("order-1", "orders.archive", 12L, 34L));
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.put(docConfig, new ArchiveLuceneIndexerSingleton.LuceneConfig(null, directory));

			@SuppressWarnings("unchecked")
			List<Object> result = (List<Object>) method.invoke(ArchiveRetriever.getInstance(), docConfig, filter, Integer.valueOf(10));

			assertTrue(result.isEmpty());
		}
		finally {
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.remove(docConfig);
		}
	}

	@Test
	void searchIndexReturnsArchiveEntriesForMatchedDocuments() throws Exception {
		ArchiveDocConfig docConfig = new ArchiveDocConfig("sales", "Order", "orders", 30);
		LuceneFilter filter = new LuceneFilter();
		filter.addEquals(Bean.DOCUMENT_ID, "order-1");
		Method method = ArchiveRetriever.class.getDeclaredMethod("searchIndex", ArchiveDocConfig.class, LuceneFilter.class, int.class);
		method.setAccessible(true);

		try (Directory directory = new ByteBuffersDirectory()) {
			writeArchiveIndex(directory,
								archiveDocument("order-1", "orders.archive", 12L, 34L),
								archiveDocument("order-2", "other.archive", 56L, 78L));
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.put(docConfig, new ArchiveLuceneIndexerSingleton.LuceneConfig(null, directory));

			@SuppressWarnings("unchecked")
			List<Object> result = (List<Object>) method.invoke(ArchiveRetriever.getInstance(), docConfig, filter, Integer.valueOf(10));

			assertEquals(1, result.size());
			Object entry = result.get(0);
			assertEquals(docConfig, invokeRecordAccessor(entry, "docConfig"));
			assertEquals("orders.archive", invokeRecordAccessor(entry, "fileName"));
			assertEquals(Long.valueOf(12L), invokeRecordAccessor(entry, "offset"));
			assertEquals(Long.valueOf(34L), invokeRecordAccessor(entry, "length"));
		}
		finally {
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.remove(docConfig);
		}
	}

	@Test
	void retrieveByBizIdReturnsEmptyWhenIndexHasNoMatchingDocument() throws Exception {
		ArchiveDocConfig docConfig = new ArchiveDocConfig("sales", "Order", "orders", 30);

		try (Directory directory = new ByteBuffersDirectory()) {
			writeArchiveIndex(directory, archiveDocument("order-1", "orders.archive", 12L, 34L));
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.put(docConfig, new ArchiveLuceneIndexerSingleton.LuceneConfig(null, directory));

			assertFalse(ArchiveRetriever.getInstance()
											.retrieveByBizId(docConfig, "missing")
											.isPresent());
		}
		finally {
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.remove(docConfig);
		}
	}

	@Test
	void retrieveByBizIdReturnsEmptyWhenIndexIsMissing() throws Exception {
		ArchiveDocConfig docConfig = new ArchiveDocConfig("sales", "Order", "orders", 30);

		try (Directory directory = new ByteBuffersDirectory()) {
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.put(docConfig, new ArchiveLuceneIndexerSingleton.LuceneConfig(null, directory));

			assertFalse(ArchiveRetriever.getInstance()
											.retrieveByBizId(docConfig, "missing")
											.isPresent());
		}
		finally {
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.remove(docConfig);
		}
	}

	@Test
	void retrieveAllReturnsEmptyWhenIndexHasNoMatchingDocuments() throws Exception {
		ArchiveDocConfig docConfig = new ArchiveDocConfig("sales", "Order", "orders", 30);
		LuceneFilter filter = new LuceneFilter();
		filter.addEquals(Bean.DOCUMENT_ID, "missing");

		try (Directory directory = new ByteBuffersDirectory()) {
			writeArchiveIndex(directory, archiveDocument("order-1", "orders.archive", 12L, 34L));
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.put(docConfig, new ArchiveLuceneIndexerSingleton.LuceneConfig(null, directory));

			assertTrue(ArchiveRetriever.getInstance()
										.retrieveAll(docConfig, filter, 10)
										.isEmpty());
		}
		finally {
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.remove(docConfig);
		}
	}

	@Test
	void retrieveAllReturnsEmptyWhenIndexIsMissing() throws Exception {
		ArchiveDocConfig docConfig = new ArchiveDocConfig("sales", "Order", "orders", 30);
		LuceneFilter filter = new LuceneFilter();
		filter.addEquals(Bean.DOCUMENT_ID, "missing");

		try (Directory directory = new ByteBuffersDirectory()) {
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.put(docConfig, new ArchiveLuceneIndexerSingleton.LuceneConfig(null, directory));

			assertTrue(ArchiveRetriever.getInstance()
										.retrieveAll(docConfig, filter, 10)
										.isEmpty());
		}
		finally {
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.remove(docConfig);
		}
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

	private static Document archiveDocument(String bizId, String fileName, long offset, long length) {
		Document document = new Document();
		document.add(new StringField(Bean.DOCUMENT_ID, bizId, Store.YES));
		document.add(new StringField(IndexArchivesJob.FILENAME_FIELD, fileName, Store.YES));
		document.add(new StoredField(IndexArchivesJob.OFFSET_FIELD, offset));
		document.add(new StoredField(IndexArchivesJob.LENGTH_FIELD, length));
		return document;
	}

	@SuppressWarnings("resource")
	private static void writeArchiveIndex(Directory directory, Document... documents) throws Exception {
		try (IndexWriter writer = new IndexWriter(directory, new IndexWriterConfig(new StandardAnalyzer()))) {
			for (Document document : documents) {
				writer.addDocument(document);
			}
			writer.commit();
		}
	}

	private static Object invokeRecordAccessor(Object entry, String name) throws Exception {
		Method method = entry.getClass()
								.getDeclaredMethod(name);
		method.setAccessible(true);
		return method.invoke(entry);
	}
}
