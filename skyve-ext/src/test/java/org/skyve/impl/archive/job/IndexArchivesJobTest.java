package org.skyve.impl.archive.job;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.store.ByteBuffersDirectory;
import org.apache.lucene.index.Term;
import org.apache.lucene.store.Directory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.archive.support.DocumentConverter;
import org.skyve.domain.Bean;
import org.skyve.impl.archive.support.ArchiveLuceneIndexerSingleton;
import org.skyve.impl.archive.support.ArchiveUtils;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UtilImpl.ArchiveConfig;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;

import jakarta.enterprise.inject.Instance;

@SuppressWarnings({"static-method", "boxing"})
class IndexArchivesJobTest {
	private final ArchiveConfig originalArchiveConfig = UtilImpl.ARCHIVE_CONFIG;
	private final String originalContentDirectory = UtilImpl.CONTENT_DIRECTORY;

	@TempDir
	Path tempDir;

	@AfterEach
	void restoreArchiveConfig() {
		UtilImpl.ARCHIVE_CONFIG = originalArchiveConfig;
		UtilImpl.CONTENT_DIRECTORY = originalContentDirectory;
	}

	@Test
	void executeStopsImmediatelyWhenBatchSizeIsInvalid() throws Exception {
		UtilImpl.ARCHIVE_CONFIG = new ArchiveConfig(10, 0, List.of(), ArchiveConfig.DISABLED.cacheConfig(),
														ArchiveConfig.DISABLED.schedule());
		IndexArchivesJob job = new IndexArchivesJob();

		job.execute();

		assertEquals(List.of("Invalid batch size configured, job ending"), job.getLog());
	}

	@Test
	void executeSkipsDocumentConfigWhenNoConverterIsAvailable() throws Exception {
		UtilImpl.CONTENT_DIRECTORY = tempDir.toString();
		ArchiveDocConfig docConfig = new ArchiveDocConfig("sales", "Order", "orders", 30);
		UtilImpl.ARCHIVE_CONFIG = new ArchiveConfig(10, 100, List.of(docConfig), ArchiveConfig.DISABLED.cacheConfig(),
														ArchiveConfig.DISABLED.schedule());
		DocumentConverter nonMatching = mock(DocumentConverter.class);
		when(nonMatching.handles("sales", "Order")).thenReturn(false);
		IndexArchivesJob job = new IndexArchivesJob();
		bindConverters(job, nonMatching);

		job.execute();

		assertEquals(List.of("Indexing " + docConfig,
								"No suitable document converter available for " + docConfig + "; skipping",
								"Index process done"),
						job.getLog());
	}

	@Test
	void executeRecordsCancellationWithoutProcessingDocumentConfigs() throws Exception {
		UtilImpl.ARCHIVE_CONFIG = new ArchiveConfig(10, 100, List.of(new ArchiveDocConfig("sales", "Order", "orders", 30)),
														ArchiveConfig.DISABLED.cacheConfig(),
														ArchiveConfig.DISABLED.schedule());
		IndexArchivesJob job = new IndexArchivesJob();

		job.cancel();
		job.execute();

		assertEquals(List.of("Index process cancelled"), job.getLog());
	}

	@Test
	void lookupConverterReturnsFirstMatchingConverter() throws Exception {
		DocumentConverter nonMatching = mock(DocumentConverter.class);
		DocumentConverter matching = mock(DocumentConverter.class);
		when(nonMatching.handles("sales", "Order")).thenReturn(false);
		when(matching.handles("sales", "Order")).thenReturn(true);
		IndexArchivesJob job = new IndexArchivesJob();
		bindConverters(job, nonMatching, matching);

		Optional<DocumentConverter> result = job.lookupConverter("sales", "Order");

		assertTrue(result.isPresent());
		assertSame(matching, result.get());
	}

	@Test
	void lookupConverterReturnsEmptyWhenNoConverterMatches() throws Exception {
		DocumentConverter nonMatching = mock(DocumentConverter.class);
		when(nonMatching.handles("sales", "Order")).thenReturn(false);
		IndexArchivesJob job = new IndexArchivesJob();
		bindConverters(job, nonMatching);

		assertTrue(job.lookupConverter("sales", "Order").isEmpty());
	}

	@Test
	void listArchiveFilesReturnsOnlyArchiveFilesInDirectory() throws Exception {
		Path first = Files.createFile(tempDir.resolve("first" + ArchiveUtils.ARCHIVE_FILE_SUFFIX));
		Path second = Files.createFile(tempDir.resolve("second" + ArchiveUtils.ARCHIVE_FILE_SUFFIX));
		Files.createFile(tempDir.resolve("notes.txt"));
		Files.createDirectories(tempDir.resolve("nested" + ArchiveUtils.ARCHIVE_FILE_SUFFIX));
		Method method = IndexArchivesJob.class.getDeclaredMethod("listArchiveFiles", Path.class);
		method.setAccessible(true);

		@SuppressWarnings("unchecked")
		List<File> result = (List<File>) method.invoke(null, tempDir);
		result.sort(Comparator.comparing(File::getName));

		assertEquals(List.of(first.toFile(), second.toFile()), result);
	}

	@Test
	void indexDocumentsProcessCreatesAnalyzer() throws Exception {
		Object process = newIndexDocumentsProcess();
		Method method = process.getClass()
								.getDeclaredMethod("newAnalyzer");
		method.setAccessible(true);

		try (Analyzer analyzer = (Analyzer) method.invoke(process)) {
			assertTrue(analyzer.toString()
								.contains("KeywordTokenizer"));
		}
	}

	@Test
	void indexDocumentsProcessCreatesBizIdTerm() throws Exception {
		Object process = newIndexDocumentsProcess();
		Method method = process.getClass()
								.getDeclaredMethod("bizIdTerm", Bean.class);
		method.setAccessible(true);
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("order-123");

		Term result = (Term) method.invoke(process, bean);

		assertEquals(Bean.DOCUMENT_ID, result.field());
		assertEquals("order-123", result.text());
	}

	@Test
	void indexDocumentsProcessWritesProgressDocument() throws Exception {
		Object process = newIndexDocumentsProcess();
		Method method = process.getClass()
								.getDeclaredMethod("updateProgress", String.class, Long.class, IndexWriter.class);
		method.setAccessible(true);
		Method newAnalyzer = process.getClass()
									.getDeclaredMethod("newAnalyzer");
		newAnalyzer.setAccessible(true);
		try (Directory directory = new ByteBuffersDirectory();
				Analyzer analyzer = (Analyzer) newAnalyzer.invoke(process);
				IndexWriter writer = new IndexWriter(directory, new IndexWriterConfig(analyzer))) {
			method.invoke(process, "orders.archive", Long.valueOf(42L), writer);
			writer.commit();

			try (DirectoryReader reader = DirectoryReader.open(directory)) {
				org.apache.lucene.document.Document document = reader.storedFields()
																		.document(0);
				assertEquals("orders.archive", document.get(IndexArchivesJob.PROGRESS_FILENAME_FIELD));
				assertEquals(42L, document.getField(IndexArchivesJob.PROGRESS_OFFSET_FIELD)
											.numericValue()
											.longValue());
			}
		}
	}

	@Test
	void indexDocumentsProcessIdentifiesArchiveFilesWithoutProgress() throws Exception {
		Path archiveDir = Files.createDirectories(tempDir.resolve("archive"));
		Path first = Files.writeString(archiveDir.resolve("first" + ArchiveUtils.ARCHIVE_FILE_SUFFIX), "one");
		Path second = Files.writeString(archiveDir.resolve("second" + ArchiveUtils.ARCHIVE_FILE_SUFFIX), "two");
		Files.writeString(archiveDir.resolve("notes.txt"), "ignored");
		ArchiveDocConfig config = new ArchiveDocConfig("sales", "Order", "orders", 30);
		Object process = newIndexDocumentsProcess(archiveDir, tempDir.resolve("index"), config);
		Method method = process.getClass()
								.getDeclaredMethod("identifyUnindexed");
		method.setAccessible(true);

		try (Directory directory = new ByteBuffersDirectory()) {
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.put(config, new ArchiveLuceneIndexerSingleton.LuceneConfig(null, directory));

			@SuppressWarnings("unchecked")
			List<Object> result = (List<Object>) method.invoke(process);

			assertEquals(List.of(first.toFile(), second.toFile()), result.stream()
																			.map(IndexArchivesJobTest::indexableFile)
																			.sorted(Comparator.comparing(File::getName))
																			.toList());
			assertTrue(result.stream()
								.allMatch(indexable -> startOffset(indexable) == 0L));
		}
		finally {
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.remove(config);
		}
	}

	@Test
	void indexDocumentsProcessIdentifiesChangedFilesFromStoredProgress() throws Exception {
		Path archiveDir = Files.createDirectories(tempDir.resolve("archive"));
		Path changed = Files.writeString(archiveDir.resolve("changed" + ArchiveUtils.ARCHIVE_FILE_SUFFIX), "alpha\nbeta");
		Files.writeString(archiveDir.resolve("unchanged" + ArchiveUtils.ARCHIVE_FILE_SUFFIX), "done");
		Path fresh = Files.writeString(archiveDir.resolve("fresh" + ArchiveUtils.ARCHIVE_FILE_SUFFIX), "new");
		ArchiveDocConfig config = new ArchiveDocConfig("sales", "Order", "orders", 30);
		Object process = newIndexDocumentsProcess(archiveDir, tempDir.resolve("index"), config);
		Method identifyUnindexed = process.getClass()
											.getDeclaredMethod("identifyUnindexed");
		identifyUnindexed.setAccessible(true);
		Method updateProgress = process.getClass()
										.getDeclaredMethod("updateProgress", String.class, Long.class, IndexWriter.class);
		updateProgress.setAccessible(true);
		Method newAnalyzer = process.getClass()
									.getDeclaredMethod("newAnalyzer");
		newAnalyzer.setAccessible(true);

		try (Directory directory = new ByteBuffersDirectory();
				Analyzer analyzer = (Analyzer) newAnalyzer.invoke(process);
				IndexWriter writer = new IndexWriter(directory, new IndexWriterConfig(analyzer))) {
			updateProgress.invoke(process, changed.getFileName().toString(), Long.valueOf(6L), writer);
			updateProgress.invoke(process, "unchanged" + ArchiveUtils.ARCHIVE_FILE_SUFFIX, Long.valueOf(4L), writer);
			writer.commit();
			ArchiveLuceneIndexerSingleton.getInstance()
											.addIndexWriter(config, writer, directory);

			@SuppressWarnings("unchecked")
			List<Object> result = (List<Object>) identifyUnindexed.invoke(process);

			assertEquals(List.of(changed.toFile(), fresh.toFile()), result.stream()
																			.map(IndexArchivesJobTest::indexableFile)
																			.sorted(Comparator.comparing(File::getName))
																			.toList());
			assertEquals(6L, result.stream()
									.filter(indexable -> indexableFile(indexable).equals(changed.toFile()))
									.mapToLong(IndexArchivesJobTest::startOffset)
									.findFirst()
									.orElseThrow());
			assertEquals(0L, result.stream()
									.filter(indexable -> indexableFile(indexable).equals(fresh.toFile()))
									.mapToLong(IndexArchivesJobTest::startOffset)
									.findFirst()
									.orElseThrow());
		}
		finally {
			ArchiveLuceneIndexerSingleton.getInstance()
											.getLuceneConfigs()
											.remove(config);
		}
	}

	private static void bindConverters(IndexArchivesJob job, DocumentConverter... converters) throws Exception {
		Instance<DocumentConverter> instance = mock(Instance.class);
		when(instance.stream()).thenAnswer(invocation -> Stream.of(converters));
		Field field = IndexArchivesJob.class.getDeclaredField("documentConverters");
		field.setAccessible(true);
		field.set(job, instance);
	}

	private Object newIndexDocumentsProcess() throws Exception {
		return newIndexDocumentsProcess(tempDir, tempDir.resolve("index"), new ArchiveDocConfig("sales", "Order", "orders", 30));
	}

	private static Object newIndexDocumentsProcess(Path archiveDir, Path indexDir, ArchiveDocConfig config) throws Exception {
		Class<?> clazz = Class.forName("org.skyve.impl.archive.job.IndexArchivesJob$IndexDocumentsProcess");
		Constructor<?> constructor = clazz.getDeclaredConstructor(IndexArchivesJob.class, Path.class, Path.class,
																	DocumentConverter.class, ArchiveDocConfig.class);
		constructor.setAccessible(true);
		return constructor.newInstance(new IndexArchivesJob(), archiveDir, indexDir,
										mock(DocumentConverter.class),
										config);
	}

	private static File indexableFile(Object indexable) {
		try {
			Method method = indexable.getClass()
										.getDeclaredMethod("file");
			method.setAccessible(true);
			return (File) method.invoke(indexable);
		}
		catch (ReflectiveOperationException e) {
			throw new IllegalStateException(e);
		}
	}

	private static long startOffset(Object indexable) {
		try {
			Method method = indexable.getClass()
										.getDeclaredMethod("startOffset");
			method.setAccessible(true);
			return ((Long) method.invoke(indexable)).longValue();
		}
		catch (ReflectiveOperationException e) {
			throw new IllegalStateException(e);
		}
	}
}
