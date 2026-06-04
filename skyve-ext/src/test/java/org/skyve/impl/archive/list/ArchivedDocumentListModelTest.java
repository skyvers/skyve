package org.skyve.impl.archive.list;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.NoSuchElementException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.Comparator;
import java.util.Iterator;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.junit.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.archive.support.ArchiveLuceneIndexerSingleton;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UtilImpl.ArchiveConfig;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.impl.web.SortParameterImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

public class ArchivedDocumentListModelTest {
	@Test
	@SuppressWarnings("static-method")
	public void fetchReturnsEmptyPageWhenNoArchiveConfigExists() throws Exception {
		TestArchivedDocumentListModel model = new TestArchivedDocumentListModel();
		model.setStartRow(0);
		model.setEndRow(10);

		Page page = model.fetch();

		assertEquals(0, page.getRows().size());
		assertNotNull(page.getSummary());
	}

	@Test
	@SuppressWarnings("static-method")
	public void fetchCreatesCountSummaryWhenSummaryIsCount() throws Exception {
		ArchiveConfig previousConfig = UtilImpl.ARCHIVE_CONFIG;
		String previousContentDirectory = UtilImpl.CONTENT_DIRECTORY;

		Path contentDirectory = Files.createTempDirectory("archive-model-count-summary");
		ArchiveDocConfig docConfig = new ArchiveDocConfig("admin", "ArchivedDoc", "adm_archived_doc", 7);
		UtilImpl.CONTENT_DIRECTORY = contentDirectory.toString();
		UtilImpl.ARCHIVE_CONFIG = new ArchiveConfig(10, 100, List.of(docConfig), ArchiveConfig.DISABLED.cacheConfig(), ArchiveConfig.DISABLED.schedule());

		ArchiveLuceneIndexerSingleton singleton = ArchiveLuceneIndexerSingleton.getInstance();
		singleton.shutdown();
		singleton.startup();

		try {
			MetaDataQueryColumn column = mock(MetaDataQueryColumn.class);
			when(column.getBinding()).thenReturn("rowCount");

			CountSummaryArchivedDocumentListModel model = new CountSummaryArchivedDocumentListModel(List.of(column));
			model.setStartRow(0);
			model.setEndRow(10);
			model.setSummary(AggregateFunction.Count);

			Page page = model.fetch();

			assertEquals(0, page.getRows().size());
			assertEquals(0, page.getTotalRows());
			assertNotNull(page.getSummary());
		}
		finally {
			singleton.shutdown();
			UtilImpl.ARCHIVE_CONFIG = previousConfig;
			UtilImpl.CONTENT_DIRECTORY = previousContentDirectory;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void fetchCreatesSummaryForUnsupportedAggregateWithoutThrowing() throws Exception {
		ArchiveConfig previousConfig = UtilImpl.ARCHIVE_CONFIG;
		String previousContentDirectory = UtilImpl.CONTENT_DIRECTORY;

		Path contentDirectory = Files.createTempDirectory("archive-model-unsupported-summary");
		ArchiveDocConfig docConfig = new ArchiveDocConfig("admin", "ArchivedDoc", "adm_archived_doc", 7);
		UtilImpl.CONTENT_DIRECTORY = contentDirectory.toString();
		UtilImpl.ARCHIVE_CONFIG = new ArchiveConfig(10, 100, List.of(docConfig), ArchiveConfig.DISABLED.cacheConfig(), ArchiveConfig.DISABLED.schedule());

		ArchiveLuceneIndexerSingleton singleton = ArchiveLuceneIndexerSingleton.getInstance();
		singleton.shutdown();
		singleton.startup();

		try {
			ConfiguredArchivedDocumentListModel model = new ConfiguredArchivedDocumentListModel();
			model.setStartRow(0);
			model.setEndRow(10);
			model.setSummary(AggregateFunction.Max);

			Page page = model.fetch();

			assertEquals(0, page.getRows().size());
			assertEquals(0, page.getTotalRows());
			assertNotNull(page.getSummary());
		}
		finally {
			singleton.shutdown();
			UtilImpl.ARCHIVE_CONFIG = previousConfig;
			UtilImpl.CONTENT_DIRECTORY = previousContentDirectory;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void getIndexDirectoryThrowsWhenArchiveDocConfigIsMissing() {
		ArchiveConfig previousConfig = UtilImpl.ARCHIVE_CONFIG;
		String previousContentDirectory = UtilImpl.CONTENT_DIRECTORY;

		UtilImpl.ARCHIVE_CONFIG = new ArchiveConfig(10, 100, List.of(), ArchiveConfig.DISABLED.cacheConfig(), ArchiveConfig.DISABLED.schedule());
		UtilImpl.CONTENT_DIRECTORY = previousContentDirectory;

		try {
			ConfiguredArchivedDocumentListModel model = new ConfiguredArchivedDocumentListModel();
			assertThrows(NoSuchElementException.class, model::getIndexDirectory);
		}
		finally {
			UtilImpl.ARCHIVE_CONFIG = previousConfig;
			UtilImpl.CONTENT_DIRECTORY = previousContentDirectory;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void getSortReturnsDefaultRelevanceWhenNoSortParameters() {
		TestArchivedDocumentListModel model = new TestArchivedDocumentListModel();
		Sort sort = model.exposeSort();
		assertSame(Sort.RELEVANCE, sort);
	}

	@Test
	@SuppressWarnings("static-method")
	public void getSortUsesSortParametersAndToSortBinding() {
		SortParameterImpl parameter = new SortParameterImpl();
		parameter.setBy("name");
		parameter.setDirection(SortDirection.descending);

		TestArchivedDocumentListModel model = new TestArchivedDocumentListModel();
		model.setSortParameters(new SortParameterImpl[] { parameter });

		SortField[] fields = model.exposeSort().getSort();
		assertEquals(1, fields.length);
		assertEquals("name_sort", fields[0].getField());
		assertTrue(fields[0].getReverse());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getFilterAndNewFilterReturnLuceneFilterInstances() {
		TestArchivedDocumentListModel model = new TestArchivedDocumentListModel();

		Filter filter = model.getFilter();
		Filter freshFilter = model.newFilter();

		assertTrue(filter instanceof LuceneFilter);
		assertTrue(freshFilter instanceof LuceneFilter);
	}

	@Test
	@SuppressWarnings("static-method")
	public void postConstructResolvesDrivingDocumentFromCustomerModule() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		org.skyve.metadata.model.document.Document document = mock(org.skyve.metadata.model.document.Document.class);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "ArchivedDoc")).thenReturn(document);

		TestArchivedDocumentListModel model = new TestArchivedDocumentListModel();
		model.postConstruct(customer, true);

		assertSame(document, model.getDrivingDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	public void updateAndRemoveAreUnsupported() {
		TestArchivedDocumentListModel model = new TestArchivedDocumentListModel();

		assertThrows(UnsupportedOperationException.class, () -> model.update("bizId", null));
		assertThrows(UnsupportedOperationException.class, () -> model.remove("bizId"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void putParameterIsNoOp() {
		TestArchivedDocumentListModel model = new TestArchivedDocumentListModel();
		model.putParameter("unused", "value");
		assertTrue(model.getFilter() instanceof LuceneFilter);
	}

	@Test
	@SuppressWarnings("static-method")
	public void fetchUsesLucenePathWhenArchiveConfigExists() throws Exception {
		ArchiveConfig previousConfig = UtilImpl.ARCHIVE_CONFIG;
		String previousContentDirectory = UtilImpl.CONTENT_DIRECTORY;

		Path contentDirectory = Files.createTempDirectory("archive-model-content");
		ArchiveDocConfig docConfig = new ArchiveDocConfig("admin", "ArchivedDoc", "adm_archived_doc", 7);
		UtilImpl.CONTENT_DIRECTORY = contentDirectory.toString();
		UtilImpl.ARCHIVE_CONFIG = new ArchiveConfig(10, 100, List.of(docConfig), ArchiveConfig.DISABLED.cacheConfig(), ArchiveConfig.DISABLED.schedule());

		ArchiveLuceneIndexerSingleton singleton = ArchiveLuceneIndexerSingleton.getInstance();
		singleton.shutdown();
		singleton.startup();

		try {
			ConfiguredArchivedDocumentListModel model = new ConfiguredArchivedDocumentListModel();
			model.setStartRow(0);
			model.setEndRow(10);

			Page page = model.fetch();

			assertEquals(0, page.getRows().size());
			assertEquals(0, page.getTotalRows());
			assertNotNull(page.getSummary());
			try (var iterable = model.iterate()) {
				assertNotNull(iterable);
			}
		}
		finally {
			singleton.shutdown();
			UtilImpl.ARCHIVE_CONFIG = previousConfig;
			UtilImpl.CONTENT_DIRECTORY = previousContentDirectory;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void fetchReturnsEmptyPageWhenIndexFilesMissing() throws Exception {
		ArchiveConfig previousConfig = UtilImpl.ARCHIVE_CONFIG;
		String previousContentDirectory = UtilImpl.CONTENT_DIRECTORY;

		Path contentDirectory = Files.createTempDirectory("archive-model-index-missing");
		ArchiveDocConfig docConfig = new ArchiveDocConfig("admin", "ArchivedDoc", "adm_archived_doc", 7);
		UtilImpl.CONTENT_DIRECTORY = contentDirectory.toString();
		UtilImpl.ARCHIVE_CONFIG = new ArchiveConfig(10, 100, List.of(docConfig), ArchiveConfig.DISABLED.cacheConfig(), ArchiveConfig.DISABLED.schedule());

		ArchiveLuceneIndexerSingleton singleton = ArchiveLuceneIndexerSingleton.getInstance();
		singleton.shutdown();
		singleton.startup();

		try {
			clearIndexDirectory(docConfig.getIndexDirectory());

			ConfiguredArchivedDocumentListModel model = new ConfiguredArchivedDocumentListModel();
			model.setStartRow(0);
			model.setEndRow(10);

			Page page = model.fetch();

			assertEquals(0, page.getRows().size());
			assertEquals(0, page.getTotalRows());
		}
		finally {
			singleton.shutdown();
			UtilImpl.ARCHIVE_CONFIG = previousConfig;
			UtilImpl.CONTENT_DIRECTORY = previousContentDirectory;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void iterateTryCloseSwallowsCloseExceptions() throws Exception {
		ArchiveConfig previousConfig = UtilImpl.ARCHIVE_CONFIG;
		String previousContentDirectory = UtilImpl.CONTENT_DIRECTORY;

		Path contentDirectory = Files.createTempDirectory("archive-model-try-close");
		ArchiveDocConfig docConfig = new ArchiveDocConfig("admin", "ArchivedDoc", "adm_archived_doc", 7);
		UtilImpl.CONTENT_DIRECTORY = contentDirectory.toString();
		UtilImpl.ARCHIVE_CONFIG = new ArchiveConfig(10, 100, List.of(docConfig), ArchiveConfig.DISABLED.cacheConfig(), ArchiveConfig.DISABLED.schedule());

		ArchiveLuceneIndexerSingleton singleton = ArchiveLuceneIndexerSingleton.getInstance();
		singleton.shutdown();
		singleton.startup();

		try (var iterable = new ConfiguredArchivedDocumentListModel().iterate()) {
			Method tryClose = iterable.getClass().getDeclaredMethod("tryClose", AutoCloseable.class);
			tryClose.setAccessible(true);
			AtomicBoolean closeAttempted = new AtomicBoolean();
			tryClose.invoke(iterable, (AutoCloseable) () -> {
				closeAttempted.set(true);
				throw new Exception("boom");
			});
			assertTrue(closeAttempted.get());
		}
		finally {
			singleton.shutdown();
			UtilImpl.ARCHIVE_CONFIG = previousConfig;
			UtilImpl.CONTENT_DIRECTORY = previousContentDirectory;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void fetchWithNonEmptyFilterUsesConfiguredQueryPath() throws Exception {
		ArchiveConfig previousConfig = UtilImpl.ARCHIVE_CONFIG;
		String previousContentDirectory = UtilImpl.CONTENT_DIRECTORY;

		Path contentDirectory = Files.createTempDirectory("archive-model-filter-query");
		ArchiveDocConfig docConfig = new ArchiveDocConfig("admin", "ArchivedDoc", "adm_archived_doc", 7);
		UtilImpl.CONTENT_DIRECTORY = contentDirectory.toString();
		UtilImpl.ARCHIVE_CONFIG = new ArchiveConfig(10, 100, List.of(docConfig), ArchiveConfig.DISABLED.cacheConfig(), ArchiveConfig.DISABLED.schedule());

		ArchiveLuceneIndexerSingleton singleton = ArchiveLuceneIndexerSingleton.getInstance();
		singleton.shutdown();
		singleton.startup();

		try {
			ConfiguredArchivedDocumentListModel model = new ConfiguredArchivedDocumentListModel();
			model.setStartRow(0);
			model.setEndRow(10);
			model.getFilter().addNotNull(Bean.DOCUMENT_ID);

			Page page = model.fetch();
			assertEquals(0, page.getRows().size());
			assertEquals(0, page.getTotalRows());
		}
		finally {
			singleton.shutdown();
			UtilImpl.ARCHIVE_CONFIG = previousConfig;
			UtilImpl.CONTENT_DIRECTORY = previousContentDirectory;
		}
	}

	@Test
	@SuppressWarnings({"static-method", "resource"})
	public void iterateNextReturnsConvertedBeanForIndexedDocument() throws Exception {
		ArchiveConfig previousConfig = UtilImpl.ARCHIVE_CONFIG;
		String previousContentDirectory = UtilImpl.CONTENT_DIRECTORY;

		Path contentDirectory = Files.createTempDirectory("archive-model-iterate-next");
		ArchiveDocConfig docConfig = new ArchiveDocConfig("admin", "ArchivedDoc", "adm_archived_doc", 7);
		UtilImpl.CONTENT_DIRECTORY = contentDirectory.toString();
		UtilImpl.ARCHIVE_CONFIG = new ArchiveConfig(10, 100, List.of(docConfig), ArchiveConfig.DISABLED.cacheConfig(), ArchiveConfig.DISABLED.schedule());

		ArchiveLuceneIndexerSingleton singleton = ArchiveLuceneIndexerSingleton.getInstance();
		singleton.shutdown();
		singleton.startup();

		try {
			IndexWriter writer = singleton.getIndexWriter(docConfig);
			assertNotNull(writer);

			Document luceneDoc = new Document();
			luceneDoc.add(new TextField(Bean.DOCUMENT_ID, "biz-1", Store.YES));
			writer.addDocument(luceneDoc);
			writer.commit();

			IteratorConfiguredArchivedDocumentListModel model = new IteratorConfiguredArchivedDocumentListModel();
			model.setStartRow(0);
			model.setEndRow(10);

			try (var iterable = model.iterate()) {
				Iterator<Bean> iterator = iterable.iterator();
				assertTrue(iterator.hasNext());
				assertNotNull(iterator.next());
				assertTrue(!iterator.hasNext());
			}
		}
		finally {
			singleton.shutdown();
			UtilImpl.ARCHIVE_CONFIG = previousConfig;
			UtilImpl.CONTENT_DIRECTORY = previousContentDirectory;
		}
	}

	@Test
	@SuppressWarnings({"static-method", "resource"})
	public void iterateNextWrapsIOExceptionFromStoredDocumentRead() throws Exception {
		ArchiveConfig previousConfig = UtilImpl.ARCHIVE_CONFIG;
		String previousContentDirectory = UtilImpl.CONTENT_DIRECTORY;

		Path contentDirectory = Files.createTempDirectory("archive-model-iterate-next-ioe");
		ArchiveDocConfig docConfig = new ArchiveDocConfig("admin", "ArchivedDoc", "adm_archived_doc", 7);
		UtilImpl.CONTENT_DIRECTORY = contentDirectory.toString();
		UtilImpl.ARCHIVE_CONFIG = new ArchiveConfig(10, 100, List.of(docConfig), ArchiveConfig.DISABLED.cacheConfig(), ArchiveConfig.DISABLED.schedule());

		ArchiveLuceneIndexerSingleton singleton = ArchiveLuceneIndexerSingleton.getInstance();
		singleton.shutdown();
		singleton.startup();

		try {
			IndexWriter writer = singleton.getIndexWriter(docConfig);
			assertNotNull(writer);

			Document luceneDoc = new Document();
			luceneDoc.add(new TextField(Bean.DOCUMENT_ID, "biz-2", Store.YES));
			writer.addDocument(luceneDoc);
			writer.commit();

			IOExceptionConfiguredArchivedDocumentListModel model = new IOExceptionConfiguredArchivedDocumentListModel();
			model.setStartRow(0);
			model.setEndRow(10);

			try (var iterable = model.iterate()) {
				Iterator<Bean> iterator = iterable.iterator();
				assertTrue(iterator.hasNext());
				RuntimeException e = assertThrows(RuntimeException.class, iterator::next);
				assertTrue(e.getMessage().contains("Unable to retrieve doc #"));
			}
		}
		finally {
			singleton.shutdown();
			UtilImpl.ARCHIVE_CONFIG = previousConfig;
			UtilImpl.CONTENT_DIRECTORY = previousContentDirectory;
		}
	}


	private static void clearIndexDirectory(Path indexDirectory) throws Exception {
		if (Files.exists(indexDirectory)) {
			try (var paths = Files.walk(indexDirectory)) {
				paths.sorted(Comparator.reverseOrder())
					 .forEach(path -> {
						 if (!path.equals(indexDirectory)) {
							 try {
								 Files.deleteIfExists(path);
							 }
							 catch (Exception e) {
								 throw new IllegalStateException("Could not delete " + path, e);
							 }
						 }
					 });
			}
		}
	}

	private static final class TestArchivedDocumentListModel extends ArchivedDocumentListModel<Bean> {
		private List<MetaDataQueryColumn> columns = List.of();

		@Override
		protected String getModule() {
			return "admin";
		}

		@Override
		protected String getDocument() {
			return "ArchivedDoc";
		}

		@Override
		protected String toSortBinding(String binding) {
			return binding + "_sort";
		}

		@Override
		protected Bean convertToBean(Document luceneDoc) {
			return null;
		}

		@Override
		protected Optional<ArchiveDocConfig> findArchiveDocumentConfig() {
			return Optional.empty();
		}

		@Override
		public List<MetaDataQueryColumn> getColumns() {
			return columns;
		}

		@Override
		public String getDescription() {
			return "Test archived list";
		}

		@Override
		public Set<String> getProjections() {
			return Set.of();
		}

		Sort exposeSort() {
			return getSort();
		}
	}

	private static final class CountSummaryArchivedDocumentListModel extends ConfiguredArchivedDocumentListModel {
		private final List<MetaDataQueryColumn> columns;

		private CountSummaryArchivedDocumentListModel(List<MetaDataQueryColumn> columns) {
			this.columns = columns;
		}

		@Override
		public List<MetaDataQueryColumn> getColumns() {
			return columns;
		}
	}

	private static class ConfiguredArchivedDocumentListModel extends ArchivedDocumentListModel<Bean> {
		@Override
		protected String getModule() {
			return "admin";
		}

		@Override
		protected String getDocument() {
			return "ArchivedDoc";
		}

		@Override
		protected String toSortBinding(String binding) {
			return binding + "_sort";
		}

		@Override
		protected Bean convertToBean(Document luceneDoc) {
			return null;
		}

		@Override
		public List<MetaDataQueryColumn> getColumns() {
			return List.of();
		}

		@Override
		public String getDescription() {
			return "Configured archived list";
		}

		@Override
		public Set<String> getProjections() {
			return Set.of();
		}
	}

	private static final class IteratorConfiguredArchivedDocumentListModel extends ConfiguredArchivedDocumentListModel {
		@Override
		protected Bean convertToBean(Document luceneDoc) {
			return mock(Bean.class);
		}
	}

	private static final class IOExceptionConfiguredArchivedDocumentListModel extends ConfiguredArchivedDocumentListModel {
		@Override
		protected Document readStoredDocument(org.apache.lucene.index.DirectoryReader reader, int docId) throws IOException {
			throw new IOException("simulated");
		}

		@Override
		protected Bean convertToBean(Document luceneDoc) {
			return mock(Bean.class);
		}
	}

}
