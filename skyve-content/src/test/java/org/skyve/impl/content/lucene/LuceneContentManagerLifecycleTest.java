package org.skyve.impl.content.lucene;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Date;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.apache.lucene.document.Document;
import org.apache.lucene.document.FieldType;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.StoredField;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.DocValuesType;
import org.apache.lucene.index.FieldInfo;
import org.apache.lucene.index.FieldInfos;
import org.apache.lucene.index.IndexOptions;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.store.Directory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable.ContentIterator;
import org.skyve.content.SearchResults;
import org.skyve.content.SearchResult;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.ManyResultsException;
import org.skyve.domain.messages.NoResultsException;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.TimeUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.domain.Bean;
import org.skyve.metadata.user.User;

/**
 * Verifies the Lucene content manager lifecycle for bean content and attachments.
 *
 * <p>The suite covers exact identifier indexing and lookup, legacy-index compatibility behaviour,
 * replacement and deletion, attachment file storage, result enumeration, malformed or missing
 * content, concurrent access, lifecycle transitions and failure cleanup. Assertions inspect both
 * the public content-manager contract and the resulting Lucene field schema where the schema is
 * itself part of the compatibility guarantee.
 */
@SuppressWarnings({ "static-method", "java:S8692" }) // system clock OK
class LuceneContentManagerLifecycleTest {
	private static final FieldType CONTENT_ID_FIELD_TYPE;

	static {
		CONTENT_ID_FIELD_TYPE = new FieldType(StringField.TYPE_STORED);
		CONTENT_ID_FIELD_TYPE.setOmitNorms(false);
		CONTENT_ID_FIELD_TYPE.freeze();
	}

	private final String originalContentDirectory = UtilImpl.CONTENT_DIRECTORY;
	private final boolean originalContentFileStorage = UtilImpl.CONTENT_FILE_STORAGE;
	private final boolean originalContentTrace = UtilImpl.CONTENT_TRACE;
	private Path tempContentDirectory;

	@AfterEach
	void restoreContentDirectory() throws Exception {
		UtilImpl.CONTENT_DIRECTORY = originalContentDirectory;
		UtilImpl.CONTENT_FILE_STORAGE = originalContentFileStorage;
		UtilImpl.CONTENT_TRACE = originalContentTrace;
		if (tempContentDirectory != null) {
			try (var stream = Files.walk(tempContentDirectory)) {
				stream.sorted(Comparator.reverseOrder()).forEach(path -> {
					try {
						Files.deleteIfExists(path);
					}
					catch (Exception e) {
						throw new IllegalStateException(e);
					}
				});
			}
		}
	}

	/** Confirms that startup rejects a content path that cannot serve as a Lucene directory. */
	@Test
	void testStartupThrowsForInvalidContentDirectory() throws Exception {
		UtilImpl.CONTENT_DIRECTORY = "/dev/null";

		try (LuceneContentManager manager = new LuceneContentManager()) {
			assertThrows(IllegalStateException.class, manager::startup);
		}
	}

	/** Confirms that failures while closing the Lucene directory are surfaced during shutdown. */
	@Test
	@SuppressWarnings("resource")
	void testShutdownWrapsDirectoryCloseFailure() throws Exception {
		Directory directory = mock(Directory.class);
		doThrow(new java.io.IOException("boom")).when(directory).close();
		setStaticField("writer", null);
		setStaticField("analyzer", null);
		setStaticField("directory", directory);

		try (LuceneContentManager manager = new LuceneContentManager()) {
			assertThrows(IllegalStateException.class, manager::shutdown);
		}
	}

	/** Confirms indexing across data-group, tracing and filesystem-storage configuration branches. */
	@Test
	void testIndexingCoversDataGroupTraceAndFileStorageBranches() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-lucene-datagroup-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();
		UtilImpl.CONTENT_FILE_STORAGE = false;
		UtilImpl.CONTENT_TRACE = true;

		try (LuceneContentManager manager = new LuceneContentManager()) {
			manager.startup();

			BeanContent beanContent = new BeanContent(samplePersistentBean("bean-dg-1", "dg-1"));
			beanContent.getProperties().put("name", "alpha");
			manager.put(beanContent);
			manager.close();

			AttachmentContent attachment = new AttachmentContent("demo",
													"admin",
													"Contact",
													"dg-1",
													"",
													"biz-attach-dg-1",
													"image")
													.attachment("sample-dg.txt", "text/plain", "payload".getBytes());
			attachment.setContentId("cid-dg-1");
			manager.put(attachment, false);
			manager.close();

			assertEquals(2L, countHits(manager));

			UtilImpl.CONTENT_FILE_STORAGE = true;
			assertNull(manager.getAttachment("missing-file-storage"));

			manager.shutdown();
		}
	}

	/** Confirms that repeated close operations are safe after pending content has been flushed. */
	@Test
	void testCloseIsIdempotentAfterContentFlush() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-lucene-close-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();

		try (LuceneContentManager manager = new LuceneContentManager()) {
			manager.startup();

			BeanContent beanContent = new BeanContent(samplePersistentBean("bean-close-1"));
			beanContent.getProperties().put("name", "alpha");
			manager.put(beanContent);

			manager.close();
			manager.close();

			assertEquals(1L, countHits(manager));
			manager.shutdown();
		}
	}

	/** Confirms exact UUID attachment lookup, replacement, enumeration and removal in one lifecycle. */
	@Test
	void testAttachmentLifecycleSupportsUuidContentId() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-lucene-inline-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();
		UtilImpl.CONTENT_FILE_STORAGE = false;

		try (LuceneContentManager manager = new LuceneContentManager()) {
			manager.startup();

			byte[] originalBytes = "first payload".getBytes();
			AttachmentContent attachment = new AttachmentContent("demo",
													"admin",
													"Contact",
													null,
													"",
													"biz-attach-1",
													"image")
													.attachment("sample.txt", "text/plain", originalBytes)
													.markup("m1");
			attachment.setContentId("691a3e3c-b84c-4483-9374-7dd3727f2e06");

			manager.put(attachment, true);
			manager.close();
			assertContentIdFieldShape();

			String contentId = attachment.getContentId();
			assertNotNull(contentId);

			AttachmentContent loaded = manager.getAttachment(contentId);
			assertNotNull(loaded);
			assertEquals("sample.txt", loaded.getFileName());
			assertEquals("m1", loaded.getMarkup());
			assertArrayEquals(originalBytes, loaded.getContentBytes());
			assertEquals(1L, countHits(manager));

			attachment.attachment("renamed.txt", "text/plain", "replacement payload".getBytes());
			attachment.setContentId(contentId);
			manager.update(attachment);
			manager.close();

			AttachmentContent updated = manager.getAttachment(contentId);
			assertNotNull(updated);
			assertEquals(contentId, updated.getContentId());
			assertEquals("renamed.txt", updated.getFileName());
			assertArrayEquals(originalBytes, updated.getContentBytes());
			assertEquals(1L, countHits(manager));

			manager.reindex(attachment, false);
			manager.close();
			assertEquals(1L, countHits(manager));

			manager.removeAttachment(contentId);
			manager.close();

			assertNull(manager.getAttachment(contentId));
			manager.shutdown();
		}
	}

	/** Confirms that attachment bytes and metadata survive restart when filesystem storage is enabled. */
	@Test
	void testAttachmentLifecycleWithFileSystemStorageAndRestart() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-lucene-file-system-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();
		UtilImpl.CONTENT_FILE_STORAGE = true;
		byte[] originalBytes = "file system payload".getBytes();
		String contentId = "be0bc5dc-a7c7-448a-adb3-d8b220ce95d5";

		try (LuceneContentManager manager = new LuceneContentManager()) {
			manager.startup();
			AttachmentContent attachment = new AttachmentContent("demo",
													"admin",
													"Contact",
													null,
													"",
													"biz-file-system",
													"image")
													.attachment("stored.txt", "text/plain", originalBytes)
													.markup("before");
			attachment.setContentId(contentId);

			manager.put(attachment, false);
			manager.close();
			AttachmentContent stored = manager.getAttachment(contentId);
			assertNotNull(stored);
			assertArrayEquals(originalBytes, stored.getContentBytes());
			assertEquals("before", stored.getMarkup());

			attachment.attachment("renamed.txt", "text/plain", "replacement payload".getBytes());
			attachment.markup("after");
			manager.update(attachment);
			manager.close();
			AttachmentContent updated = manager.getAttachment(contentId);
			assertNotNull(updated);
			assertEquals("renamed.txt", updated.getFileName());
			assertEquals("after", updated.getMarkup());
			assertArrayEquals(originalBytes, updated.getContentBytes());
			assertEquals(1L, countHits(manager));

			manager.reindex(attachment, false);
			manager.close();
			assertEquals(1L, countHits(manager));
			manager.shutdown();
		}

		try (LuceneContentManager restarted = new LuceneContentManager()) {
			restarted.startup();
			AttachmentContent restored = restarted.getAttachment(contentId);
			assertNotNull(restored);
			assertArrayEquals(originalBytes, restored.getContentBytes());
			assertEquals(1L, countHits(restarted));

			restarted.removeAttachment(contentId);
			restarted.close();
			assertNull(restarted.getAttachment(contentId));
			assertEquals(0L, countHits(restarted));
			restarted.shutdown();
		}
	}

	/** Confirms that the incompatible tokenised legacy identifier schema is rejected before rebuilding. */
	@Test
	void testLegacyContentIdSchemaRequiresDropBeforeRebuild() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-lucene-legacy-schema-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();
		UtilImpl.CONTENT_FILE_STORAGE = false;
		String contentId = "65c726c3-2005-42bc-961d-fd5cc088351a";

		try (LuceneContentManager legacy = new LuceneContentManager()) {
			legacy.startup();
			addRawDocument(rawLegacyAttachmentDoc(contentId, "legacy-biz-id"));
			legacy.close();
			legacy.shutdown();
		}

		try (LuceneContentManager manager = new LuceneContentManager()) {
			manager.startup();
			assertNull(manager.getAttachment(contentId));

			AttachmentContent replacement = new AttachmentContent("demo",
													"admin",
													"Contact",
													null,
													"",
													"replacement-biz-id",
													"image")
													.attachment("replacement.txt", "text/plain", new byte[] {7});
			replacement.setContentId(contentId);
			assertThrows(IllegalArgumentException.class, () -> manager.put(replacement, false));

			manager.dropIndexing();
			manager.put(replacement, false);
			manager.close();
			assertNotNull(manager.getAttachment(contentId));
			assertEquals(1L, countHits(manager));
			manager.shutdown();
		}
	}

	/** Confirms that rolled-back attachment changes do not reappear after the manager restarts. */
	@Test
	void testUncommittedAttachmentDoesNotSurviveRollbackAndRestart() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-lucene-rollback-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();
		UtilImpl.CONTENT_FILE_STORAGE = false;
		String contentId = "41beb3ae-2d4b-41a2-b19b-fb0519c24dc8";

		try (LuceneContentManager manager = new LuceneContentManager()) {
			manager.startup();
			manager.close();
			AttachmentContent attachment = new AttachmentContent("demo",
													"admin",
													"Contact",
													null,
													"",
													"rollback-biz-id",
													"image")
													.attachment("rollback.txt", "text/plain", new byte[] {8});
			attachment.setContentId(contentId);
			manager.put(attachment, false);
			rollbackIndexWriter();
			manager.shutdown();
		}

		try (LuceneContentManager restarted = new LuceneContentManager()) {
			restarted.startup();
			assertNull(restarted.getAttachment(contentId));
			assertEquals(0L, countHits(restarted));
			restarted.shutdown();
		}
	}

	/** Confirms that concurrent replacements leave exactly one Lucene document for a content ID. */
	@Test
	void testConcurrentUpdatesPreserveSingleContentIdDocument() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-lucene-concurrent-update-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();
		UtilImpl.CONTENT_FILE_STORAGE = false;
		String contentId = "f10921c5-da7f-4e1c-949a-8af3bfb23955";
		byte[] originalBytes = new byte[] {1, 2, 3};

		try (LuceneContentManager manager = new LuceneContentManager()) {
			manager.startup();
			AttachmentContent original = new AttachmentContent("demo",
												"admin",
												"Contact",
												null,
												"",
												"concurrent-biz-id",
												"image")
												.attachment("original.txt", "text/plain", originalBytes);
			original.setContentId(contentId);
			manager.put(original, false);
			manager.close();

			AttachmentContent first = new AttachmentContent("demo",
											"admin",
											"Contact",
											null,
											"",
											"concurrent-biz-id",
											"image")
											.attachment("first.txt", "text/plain", new byte[] {4});
			first.setContentId(contentId);
			AttachmentContent second = new AttachmentContent("demo",
											 "admin",
											 "Contact",
											 null,
											 "",
											 "concurrent-biz-id",
											 "image")
											 .attachment("second.txt", "text/plain", new byte[] {5});
			second.setContentId(contentId);

			ExecutorService executor = Executors.newFixedThreadPool(2);
			try {
				List<Future<Void>> updates = executor.invokeAll(List.of(() -> {
					manager.update(first);
					return null;
				}, () -> {
					manager.update(second);
					return null;
				}));
				for (Future<Void> update : updates) {
					update.get();
				}
			}
			finally {
				executor.shutdownNow();
			}

			manager.close();
			assertEquals(1L, countHits(manager));
			AttachmentContent updated = manager.getAttachment(contentId);
			assertNotNull(updated);
			assertTrue("first.txt".equals(updated.getFileName()) || "second.txt".equals(updated.getFileName()));
			assertArrayEquals(originalBytes, updated.getContentBytes());
			manager.shutdown();
		}
	}

	/** Confirms that truncate clears indexed content and drop removes the index for clean recreation. */
	@SuppressWarnings("null") // uses a nullable result where the API expects non-null
	@Test
	void testTruncateAndDropIndexing() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-lucene-truncate-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();
		UtilImpl.CONTENT_FILE_STORAGE = false;

		try (LuceneContentManager manager = new LuceneContentManager()) {
			manager.startup();

			AttachmentContent demoAttachment = new AttachmentContent("demo",
														"admin",
														"Contact",
														null,
														"",
														"biz-demo-a",
														"image").attachment("d.txt", "text/plain", new byte[] {1});
			demoAttachment.setContentId("ciddemo001");
			AttachmentContent otherAttachment = new AttachmentContent("other",
														 "admin",
														 "Contact",
														 null,
														 "",
														 "biz-other-a",
														 "image").attachment("o.txt", "text/plain", new byte[] {2});
			otherAttachment.setContentId("cidother001");

			manager.put(demoAttachment, false);
			manager.put(otherAttachment, false);
			BeanContent demoBean = new BeanContent(samplePersistentBean("bean-demo"));
			demoBean.getProperties().put("name", "alpha demo");
			BeanContent otherBean = new BeanContent(samplePersistentBean("bean-other"));
			otherBean.getProperties().put("name", "alpha other");
			manager.put(demoBean);
			manager.put(otherBean);
			manager.close();

			manager.truncateAttachmentIndexing("demo");
			manager.close();
			assertNull(manager.getAttachment(demoAttachment.getContentId()));
			assertNotNull(manager.getAttachment(otherAttachment.getContentId()));

			manager.truncateBeanIndexing("demo");
			manager.close();
			assertEquals(1L, countHits(manager));

			manager.truncateIndexing("other");
			manager.close();
			assertFalse(manager.all().iterator().hasNext());

			manager.dropIndexing();
			manager.close();
			assertFalse(manager.all().iterator().hasNext());

			manager.shutdown();
		}
	}

	/** Confirms that update and retrieval failures follow the content manager's documented exceptions. */
	@Test
	void testUpdateAndGetAttachmentExceptionBranches() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-lucene-branches-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();
		UtilImpl.CONTENT_FILE_STORAGE = false;

		try (LuceneContentManager manager = new LuceneContentManager()) {
			manager.startup();

			AttachmentContent existing = new AttachmentContent("demo",
													  "admin",
													  "Contact",
													  null,
													  "",
													  "biz-existing",
													  "image").attachment("ok.txt", "text/plain", new byte[] {4});
			existing.setContentId("cidexisting01");
			manager.put(existing, false);
			manager.close();

			AttachmentContent missing = new AttachmentContent("demo",
													 "admin",
													 "Contact",
													 null,
													 "",
													 "biz-missing",
													 "image").attachment("x.txt", "text/plain", new byte[] {3});
			missing.setContentId("cidmissing01");

			assertThrows(NoResultsException.class, () -> manager.update(missing));

			addRawDocument(rawAttachmentDoc("dupcontentid", "bizdup1", true));
			addRawDocument(rawAttachmentDoc("dupcontentid", "bizdup2", true));
			addRawDocument(rawAttachmentDoc("nobytesid", "biznobytes", false));
			manager.close();

			assertThrows(ManyResultsException.class, () -> manager.getAttachment("dupcontentid"));
			missing.setContentId("dupcontentid");
			assertThrows(ManyResultsException.class, () -> manager.update(missing));
			assertNull(manager.getAttachment("nobytesid"));

			manager.shutdown();
		}
	}

	/** Confirms bean-content search results and enforcement of requested and maximum result limits. */
	@SuppressWarnings("null") // a null here fails the test
	@Test
	void testGoogleSearchFindsBeanContentAndHonoursLimits() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-lucene-google-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();
		UtilImpl.CONTENT_FILE_STORAGE = false;
		setSuperUserForThread();

		try (LuceneContentManager manager = new LuceneContentManager()) {
			manager.startup();

			BeanContent first = new BeanContent(samplePersistentBean("bean-google-1"));
			first.getProperties().put("name", "alpha bravo");
			first.getProperties().put("note", "charlie delta");
			BeanContent second = new BeanContent(samplePersistentBean("bean-google-2"));
			second.getProperties().put("name", "alpha echo");

			manager.put(first);
			manager.put(second);
			manager.close();

			SearchResults limited = manager.google("alpha", 1);
			assertEquals(1, limited.getResults().size());
			SearchResult result = limited.getResults().get(0);
			assertTrue(result.getBizId().startsWith("bean-google-"));
			assertEquals("admin", result.getModuleName());
			assertEquals("Contact", result.getDocumentName());
			assertEquals("demo", result.getCustomerName());
			assertNull(result.getContentId());
			assertNotNull(result.getExcerpt());
			assertNotNull(limited.getSearchTimeInSecs());

			assertTrue(manager.google("   ", 1).getResults().isEmpty());
			assertTrue(manager.google("alpha", 0).getResults().isEmpty());

			manager.shutdown();
		}
		finally {
			clearThreadPersistence();
		}
	}

	/** Confirms full-text search returns matching attachment metadata and content identifiers. */
	@Test
	void testGoogleSearchFindsAttachmentContent() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-lucene-google-attachment-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();
		UtilImpl.CONTENT_FILE_STORAGE = false;
		setSuperUserForThread();

		try (LuceneContentManager manager = new LuceneContentManager()) {
			manager.startup();

			AttachmentContent attachment = new AttachmentContent("demo",
													"admin",
													"Contact",
													null,
													"",
													"biz-attachment-google-1",
													"image")
													.attachment("google.txt", "text/plain", "alpha attachment body".getBytes());
			attachment.setContentId("cidgoogatt01");
			manager.put(attachment, true);
			manager.close();

			SearchResults results = manager.google("alpha", 5);
			assertEquals(1, results.getResults().size());
			SearchResult result = results.getResults().get(0);
			assertEquals("cidgoogatt01", result.getContentId());
			assertEquals("biz-attachment-google-1", result.getBizId());
			assertEquals("image", result.getAttributeName());
			assertEquals("admin", result.getModuleName());
			assertEquals("Contact", result.getDocumentName());
			assertNotNull(result.getLastModified());

			manager.shutdown();
		}
		finally {
			clearThreadPersistence();
		}
	}

	/** Confirms search excludes results whose documents are not accessible to the current user. */
	@Test
	@SuppressWarnings("boxing")
	void testGoogleSearchFiltersInaccessibleContent() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-lucene-google-filter-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();
		UtilImpl.CONTENT_FILE_STORAGE = false;
		User user = mock(User.class);
		when(user.canReadBean("bean-filter-1~", "admin", "Contact", "demo", null, "")).thenReturn(Boolean.FALSE);
		setUserForThread(user);

		try (LuceneContentManager manager = new LuceneContentManager()) {
			manager.startup();

			BeanContent bean = new BeanContent(samplePersistentBean("bean-filter-1"));
			bean.getProperties().put("name", "alpha hidden");
			manager.put(bean);
			manager.close();

			assertTrue(manager.google("alpha", 5).getResults().isEmpty());

			manager.shutdown();
		}
		finally {
			clearThreadPersistence();
		}
	}

	private static Document rawAttachmentDoc(String contentId, String bizId, boolean includeAttachment) {
		Document d = new Document();
		d.add(new StringField(Bean.CUSTOMER_NAME, "demo", Store.YES));
		d.add(new StringField(Bean.MODULE_KEY, "admin", Store.YES));
		d.add(new StringField(Bean.DOCUMENT_KEY, "Contact", Store.YES));
		d.add(new StringField(Bean.DOCUMENT_ID, bizId, Store.YES));
		d.add(new StoredField(AbstractContentManager.LAST_MODIFIED, TimeUtil.formatISODate(new Date(), true)));
		d.add(new org.apache.lucene.document.Field(AbstractContentManager.CONTENT_ID,
														contentId,
														CONTENT_ID_FIELD_TYPE));
		if (includeAttachment) {
			d.add(new StoredField("attachment", new byte[] {9, 9}));
		}
		return d;
	}

	private static Document rawLegacyAttachmentDoc(String contentId, String bizId) {
		Document document = rawAttachmentDoc(contentId, bizId, true);
		document.removeField(AbstractContentManager.CONTENT_ID);
		document.add(new TextField(AbstractContentManager.CONTENT_ID, contentId, Store.YES));
		return document;
	}

	private static void addRawDocument(Document document) throws Exception {
		Field writerField = LuceneContentManager.class.getDeclaredField("writer");
		writerField.setAccessible(true);
		IndexWriter indexWriter = (IndexWriter) writerField.get(null);
		indexWriter.addDocument(document);
	}

	private static void rollbackIndexWriter() throws Exception {
		Field writerField = LuceneContentManager.class.getDeclaredField("writer");
		writerField.setAccessible(true);
		IndexWriter indexWriter = (IndexWriter) writerField.get(null);
		indexWriter.rollback();
	}

	private static void assertContentIdFieldShape() throws Exception {
		Field directoryField = LuceneContentManager.class.getDeclaredField("directory");
		directoryField.setAccessible(true);
		Directory indexDirectory = (Directory) directoryField.get(null);
		try (DirectoryReader reader = DirectoryReader.open(indexDirectory)) {
			FieldInfo contentId = FieldInfos.getMergedFieldInfos(reader).fieldInfo(AbstractContentManager.CONTENT_ID);
			assertNotNull(contentId);
			assertEquals(IndexOptions.DOCS, contentId.getIndexOptions());
			assertEquals(DocValuesType.NONE, contentId.getDocValuesType());
			assertTrue(contentId.hasNorms());
		}
	}

	private static long countHits(LuceneContentManager manager) throws Exception {
		long result = 0;
		ContentIterator it = manager.all().iterator();
		while (it.hasNext()) {
			it.next();
			result++;
		}
		return result;
	}

	private static void setSuperUserForThread() {
		SuperUser user = new SuperUser();
		setUserForThread(user);
	}

	private static void setUserForThread(User user) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}

	/** Confirms bean content can be indexed, replaced, retrieved, enumerated and removed by business ID. */
	@Test
	void testBeanIndexLifecycle() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-lucene-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();

		try (LuceneContentManager manager = new LuceneContentManager()) {
			manager.startup();

			String bizId = "bean-123";
			BeanContent beanContent = new BeanContent(samplePersistentBean(bizId));
			beanContent.getProperties().put("name", "alpha");
			beanContent.getProperties().put("ignored", null);

			manager.put(beanContent);
			manager.close();

			ContentIterator iterator = manager.all().iterator();
			assertTrue(iterator.hasNext());
			SearchResult result = iterator.next();
			assertEquals(bizId, result.getBizId());
			assertFalse(result.isAttachment());

			manager.removeBean(bizId);
			manager.close();

			ContentIterator afterRemove = manager.all().iterator();
			assertFalse(afterRemove.hasNext());

			manager.shutdown();
		}
	}

	private static PersistentBean samplePersistentBean(String bizId) {
		return samplePersistentBean(bizId, null);
	}

	private static PersistentBean samplePersistentBean(String bizId, String bizDataGroupId) {
		InvocationHandler handler = (proxy, method, args) -> {
			String name = method.getName();
			return switch (name) {
				case "getBizCustomer" -> "demo";
				case "getBizModule" -> "admin";
				case "getBizDocument" -> "Contact";
				case "getBizDataGroupId" -> bizDataGroupId;
				case "getBizUserId" -> "";
				case "getBizId" -> bizId;
				case "isPersisted" -> Boolean.TRUE;
				case "equals" -> Boolean.valueOf(proxy == args[0]);
				case "hashCode" -> Integer.valueOf(System.identityHashCode(proxy));
				case "toString" -> "PersistentBeanProxy(" + bizId + ")";
				default -> null;
			};
		};

		return (PersistentBean) Proxy.newProxyInstance(PersistentBean.class.getClassLoader(), new Class<?>[] {PersistentBean.class}, handler);
	}

	private static void setStaticField(String name, Object value) throws Exception {
		Field field = LuceneContentManager.class.getDeclaredField(name);
		field.setAccessible(true);
		field.set(null, value);
	}
}
