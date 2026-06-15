package org.skyve.impl.content.lucene;

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

import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.StoredField;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
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

@SuppressWarnings({ "static-method", "java:S8692" }) // system clock OK
class LuceneContentManagerLifecycleTest {
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

	@Test
	void testStartupThrowsForInvalidContentDirectory() throws Exception {
		UtilImpl.CONTENT_DIRECTORY = "/dev/null";

		try (LuceneContentManager manager = new LuceneContentManager()) {
			assertThrows(IllegalStateException.class, manager::startup);
		}
	}

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

	@Test
	void testAttachmentLifecycleWithInlineStorage() throws Exception {
		tempContentDirectory = Files.createTempDirectory("skyve-content-lucene-inline-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();
		UtilImpl.CONTENT_FILE_STORAGE = false;

		try (LuceneContentManager manager = new LuceneContentManager()) {
			manager.startup();

			AttachmentContent attachment = new AttachmentContent("demo",
													"admin",
													"Contact",
													null,
													"",
													"biz-attach-1",
													"image")
													.attachment("sample.txt", "text/plain", "first payload".getBytes())
													.markup("m1");
			attachment.setContentId("cidattach001");

			manager.put(attachment, true);
			manager.close();

			String contentId = attachment.getContentId();
			assertNotNull(contentId);

			AttachmentContent loaded = manager.getAttachment(contentId);
			assertNotNull(loaded);
			assertEquals("sample.txt", loaded.getFileName());
			assertEquals("m1", loaded.getMarkup());

			attachment.attachment("sample.txt", "text/plain", "second payload".getBytes());
			attachment.setContentId(contentId);
			manager.update(attachment);
			manager.close();

			AttachmentContent updated = manager.getAttachment(contentId);
			assertNotNull(updated);
			assertEquals(contentId, updated.getContentId());

			manager.reindex(attachment, false);
			manager.close();

			manager.removeAttachment(contentId);
			manager.close();

			assertNull(manager.getAttachment(contentId));
			manager.shutdown();
		}
	}

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
		d.add(new TextField(AbstractContentManager.CONTENT_ID, contentId, Store.YES));
		if (includeAttachment) {
			d.add(new StoredField("attachment", new byte[] {9, 9}));
		}
		return d;
	}

	private static void addRawDocument(Document document) throws Exception {
		Field writerField = LuceneContentManager.class.getDeclaredField("writer");
		writerField.setAccessible(true);
		IndexWriter indexWriter = (IndexWriter) writerField.get(null);
		indexWriter.addDocument(document);
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