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
import org.apache.lucene.index.Term;
import org.apache.lucene.store.ByteBuffersDirectory;
import org.apache.lucene.store.Directory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.archive.support.DocumentConverter;
import org.skyve.domain.Bean;
import org.skyve.impl.archive.support.ArchiveUtils;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;

import jakarta.enterprise.inject.Instance;

@SuppressWarnings({"static-method", "boxing"})
class IndexArchivesJobTest {
	@TempDir
	Path tempDir;

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

	private static void bindConverters(IndexArchivesJob job, DocumentConverter... converters) throws Exception {
		Instance<DocumentConverter> instance = mock(Instance.class);
		when(instance.stream()).thenAnswer(invocation -> Stream.of(converters));
		Field field = IndexArchivesJob.class.getDeclaredField("documentConverters");
		field.setAccessible(true);
		field.set(job, instance);
	}

	private Object newIndexDocumentsProcess() throws Exception {
		Class<?> clazz = Class.forName("org.skyve.impl.archive.job.IndexArchivesJob$IndexDocumentsProcess");
		Constructor<?> constructor = clazz.getDeclaredConstructor(IndexArchivesJob.class, Path.class, Path.class,
																	DocumentConverter.class, ArchiveDocConfig.class);
		constructor.setAccessible(true);
		return constructor.newInstance(new IndexArchivesJob(), tempDir, tempDir.resolve("index"),
										mock(DocumentConverter.class),
										new ArchiveDocConfig("sales", "Order", "orders", 30));
	}
}
