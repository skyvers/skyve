package org.skyve.impl.job;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.lang.reflect.Field;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.TimeUnit;

import org.quartz.JobExecutionException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResult;
import org.skyve.domain.Bean;
import org.skyve.domain.app.AppConstants;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.metadata.model.document.field.Content;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.SQL;

/**
 * Verifies the decision-making and failure handling of content garbage collection.
 *
 * <p>The tests distinguish attachment content from bean content and exercise the age guard,
 * persistent ownership checks, dynamic and static content bindings, dynamic cross-references,
 * collection limits, deferred removal, tracing paths and transaction completion. Test doubles
 * are installed through Skyve's existing content-manager, repository and thread-bound persistence
 * contracts; the production job exposes no test-specific hooks.
 *
 * <p>The complementary {@code LuceneContentGarbageCollectionJobTest} verifies that these decisions
 * produce the expected changes in a real Lucene index and file store.
 */
class ContentGarbageCollectionJobTest {
	private Class<? extends AbstractContentManager> originalContentManagerClass;
	private ProvidedRepository originalRepository;
	private boolean originalContentTrace;
	private boolean originalThumbnailFileStorage;
	private String originalThumbnailDirectory;
	private AbstractPersistence persistence;

	@BeforeEach
	void setup() throws Exception {
		originalContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;
		originalRepository = ProvidedRepositoryFactory.get();
		originalContentTrace = org.skyve.impl.util.UtilImpl.CONTENT_TRACE;
		originalThumbnailFileStorage = org.skyve.impl.util.UtilImpl.THUMBNAIL_FILE_STORAGE;
		originalThumbnailDirectory = org.skyve.impl.util.UtilImpl.THUMBNAIL_DIRECTORY;
		// So running the job never sweeps a real thumbnail directory
		org.skyve.impl.util.UtilImpl.THUMBNAIL_FILE_STORAGE = false;
		AbstractContentManager.IMPLEMENTATION_CLASS = RecordingContentManager.class;
		RecordingContentManager.reset();
		persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);
	}

	@AfterEach
	void teardown() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = originalContentManagerClass;
		if (originalRepository == null) {
			ProvidedRepositoryFactory.clear();
		}
		else {
			ProvidedRepositoryFactory.set(originalRepository);
		}
		org.skyve.impl.util.UtilImpl.CONTENT_TRACE = originalContentTrace;
		org.skyve.impl.util.UtilImpl.THUMBNAIL_FILE_STORAGE = originalThumbnailFileStorage;
		org.skyve.impl.util.UtilImpl.THUMBNAIL_DIRECTORY = originalThumbnailDirectory;
		unbindPersistenceFromThread();
	}

	/** Confirms that an empty scan still completes the job transaction. */
	@Test
	void executeCommitsWhenThereIsNoContentToCollect() {
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();

		assertDoesNotThrow(() -> job.execute(null));
		verify(persistence).commit(true);
	}

	/** Confirms that content with no modification timestamp is never considered safe to delete. */
	@Test
	void executeSkipsAttachmentAndBeanContentWhenLastModifiedIsUnknown() {
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		SearchResult attachment = searchResult("attachmentBizId", "contentId", "attachment");
		SearchResult bean = searchResult("beanBizId", null, null);
		RecordingContentManager.results = List.of(attachment, bean);

		assertDoesNotThrow(() -> job.execute(null));

		assertTrue(RecordingContentManager.removedAttachments.isEmpty());
		assertTrue(RecordingContentManager.removedBeans.isEmpty());
		verify(persistence).commit(true);
	}

	/** Confirms that the minimum-age guard protects recently modified attachments and bean content. */
	@Test
	void executeSkipsRecentlyModifiedAttachmentAndBeanContent() {
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		SearchResult attachment = searchResult("attachmentBizId", "contentId", "attachment");
		SearchResult bean = searchResult("beanBizId", null, null);
		attachment.setLastModified(new Date());
		bean.setLastModified(new Date());
		RecordingContentManager.results = List.of(attachment, bean);

		assertDoesNotThrow(() -> job.execute(null));

		assertTrue(RecordingContentManager.removedAttachments.isEmpty());
		assertTrue(RecordingContentManager.removedBeans.isEmpty());
		verify(persistence).commit(true);
	}

	/** Confirms that orphans found on a prior pass are removed and not retained for another pass. */
	@Test
	void executeRemovesPreviouslyRecordedOrphanedContentAndClearsTrackingSets() throws Exception {
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		setTrackingSet(job, "orphanedAttachmentContentIds", new TreeSet<>(Set.of("content-1")));
		setTrackingSet(job, "orphanedBeanBizIds", new TreeSet<>(Set.of("bean-1")));

		assertDoesNotThrow(() -> job.execute(null));

		assertTrue(RecordingContentManager.removedAttachments.contains("content-1"));
		assertTrue(RecordingContentManager.removedBeans.contains("bean-1"));
		assertTrue(trackingSet(job, "orphanedAttachmentContentIds").isEmpty());
		assertTrue(trackingSet(job, "orphanedBeanBizIds").isEmpty());
		verify(persistence).commit(true);
	}

	/** Confirms that old stored thumbnails are swept at most once a day. */
	@Test
	@SuppressWarnings("static-method")
	void sweepOldThumbnailsDeletesOldThumbnailsAtMostOnceADay(@TempDir File thumbnails) throws Exception {
		org.skyve.impl.util.UtilImpl.THUMBNAIL_FILE_STORAGE = true;
		org.skyve.impl.util.UtilImpl.THUMBNAIL_DIRECTORY = thumbnails.getAbsolutePath() + '/';
		// Later than any sweep before it in this JVM
		long now = System.currentTimeMillis() + TimeUnit.DAYS.toMillis(1000);

		File first = oldThumbnail(thumbnails, "first");
		ContentGarbageCollectionJob.sweepOldThumbnails(now);
		assertFalse(first.exists());

		File second = oldThumbnail(thumbnails, "second");
		ContentGarbageCollectionJob.sweepOldThumbnails(now + TimeUnit.HOURS.toMillis(23));
		assertTrue(second.exists(), "swept less than a day ago");
		ContentGarbageCollectionJob.sweepOldThumbnails(now + TimeUnit.HOURS.toMillis(25));
		assertFalse(second.exists());
	}

	/** Makes a stored thumbnail a year old, well past the retention period. */
	private static File oldThumbnail(File thumbnails, String folder) throws Exception {
		File result = new File(thumbnails, folder + "/128x128.png");
		assertTrue(result.getParentFile().mkdirs());
		assertTrue(result.createNewFile());
		long old = System.currentTimeMillis() - TimeUnit.DAYS.toMillis(365);
		assertTrue(result.setLastModified(old));
		return result;
	}

	/** Confirms that one failed removal does not abort collection or leave stale tracking entries. */
	@Test
	void executeSwallowsRemovalFailuresAndStillClearsTrackingSets() throws Exception {
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		setTrackingSet(job, "orphanedAttachmentContentIds", new TreeSet<>(Set.of("content-1")));
		setTrackingSet(job, "orphanedBeanBizIds", new TreeSet<>(Set.of("bean-1")));
		RecordingContentManager.failRemovals = true;

		assertDoesNotThrow(() -> job.execute(null));

		assertTrue(trackingSet(job, "orphanedAttachmentContentIds").isEmpty());
		assertTrue(trackingSet(job, "orphanedBeanBizIds").isEmpty());
		verify(persistence).commit(true);
	}

	/** Confirms that diagnostic tracing does not alter recovery from failed removals. */
	@Test
	void executeTracesRemovalFailuresAndStillClearsTrackingSets() throws Exception {
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		setTrackingSet(job, "orphanedAttachmentContentIds", new TreeSet<>(Set.of("content-1")));
		setTrackingSet(job, "orphanedBeanBizIds", new TreeSet<>(Set.of("bean-1")));
		RecordingContentManager.failRemovals = true;
		org.skyve.impl.util.UtilImpl.CONTENT_TRACE = true;

		assertDoesNotThrow(() -> job.execute(null));

		assertTrue(trackingSet(job, "orphanedAttachmentContentIds").isEmpty());
		assertTrue(trackingSet(job, "orphanedBeanBizIds").isEmpty());
		verify(persistence).commit(true);
	}

	/** Confirms that an index-enumeration failure is reported after the transaction is completed. */
	@Test
	void executeWrapsContentEnumerationFailureAndStillCommits() {
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		RecordingContentManager.failAll = true;

		assertThrows(JobExecutionException.class, () -> job.execute(null));

		verify(persistence).commit(true);
	}

	/** Confirms that a persistence commit failure is exposed as a job execution failure. */
	@Test
	void executeWrapsCommitFailure() {
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		doThrow(new IllegalStateException("commit failed")).when(persistence).commit(true);

		assertThrows(JobExecutionException.class, () -> job.execute(null));

		verify(persistence).commit(true);
	}

	/** Confirms that content for a transient document is ignored because it has no persistent owner. */
	@Test
	void executeSkipsEligibleContentWhenDocumentIsNotPersistent() {
		ProvidedRepositoryFactory.set(repositoryWithPersistent(null));
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		RecordingContentManager.results = List.of(oldSearchResult("bean-1", null, null),
													oldSearchResult("attachment-1", "content-1", "file"));

		assertDoesNotThrow(() -> job.execute(null));

		assertTrue(RecordingContentManager.removedAttachments.isEmpty());
		assertTrue(RecordingContentManager.removedBeans.isEmpty());
		verify(persistence).commit(true);
	}

	/** Confirms that inherited-only persistence without a physical identifier is not queried. */
	@Test
	void executeSkipsEligibleContentWhenDocumentIsNotDirectlyPersistent() {
		Persistent persistent = mock(Persistent.class);
		when(persistent.getPersistentIdentifier()).thenReturn(null);
		ProvidedRepositoryFactory.set(repositoryWithPersistent(persistent));
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		RecordingContentManager.results = List.of(oldSearchResult("bean-1", null, null),
													oldSearchResult("attachment-1", "content-1", "file"));

		assertDoesNotThrow(() -> job.execute(null));

		assertTrue(RecordingContentManager.removedAttachments.isEmpty());
		assertTrue(RecordingContentManager.removedBeans.isEmpty());
		verify(persistence).commit(true);
	}

	/** Confirms that old bean content is collected when its persistent bean row no longer exists. */
	@Test
	void executeRemovesEligibleBeanContentWhenPersistentRowIsMissing() {
		SQL sql = mock(SQL.class);
		when(persistence.newSQL(anyString())).thenReturn(sql);
		when(sql.scalarResults(Integer.class)).thenReturn(Collections.emptyList());
		ProvidedRepositoryFactory.set(repositoryWithPersistent(persistent("TEST_TABLE")));
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		RecordingContentManager.results = List.of(oldSearchResult("bean-1", null, null));

		assertDoesNotThrow(() -> job.execute(null));

		verify(sql).putParameter(Bean.DOCUMENT_ID, "bean-1", false);
		assertTrue(RecordingContentManager.removedBeans.contains("bean-1"));
		verify(persistence).commit(true);
	}

	/** Confirms that an attachment remains when its owning bean row still names its content ID. */
	@Test
	void executeKeepsEligibleAttachmentContentWhenPersistentRowStillExists() {
		SQL sql = mock(SQL.class);
		when(persistence.newSQL(anyString())).thenReturn(sql);
		when(sql.scalarResults(Integer.class)).thenReturn(List.of(Integer.valueOf(1)));
		ProvidedRepositoryFactory.set(repositoryWithPersistent(persistent("TEST_TABLE")));
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		RecordingContentManager.results = List.of(oldSearchResult("bean-1", "content-1", "attachment"));

		assertDoesNotThrow(() -> job.execute(null));

		verify(sql).putParameter(Bean.DOCUMENT_ID, "bean-1", false);
		verify(sql).putParameter("attachment", "content-1", false);
		assertTrue(RecordingContentManager.removedAttachments.isEmpty());
		verify(persistence).commit(true);
	}

	/** Confirms that an old attachment is collected only when neither ownership nor a cross-reference exists. */
	@Test
	void executeRemovesEligibleAttachmentWhenNoOwningOrCrossReferenceExists() {
		metadataContext(persistent("TEST_TABLE"));
		SQL ownershipQuery = sqlReturningRows(Collections.emptyList());
		SQL dynamicReferenceQuery = dynamicReferenceQuery(List.of());
		when(persistence.newSQL("select 1 from TEST_TABLE where bizId = :bizId and attachment = :attachment"))
				.thenReturn(ownershipQuery);
		when(persistence.newSQL("select bizId, bizCustomer, moduleName, documentName, fields from DYN_ENTITY where fields like :like"))
				.thenReturn(dynamicReferenceQuery);
		org.skyve.impl.util.UtilImpl.CONTENT_TRACE = true;
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		RecordingContentManager.results = List.of(oldSearchResult("bean-1", "content-1", "attachment"));

		assertDoesNotThrow(() -> job.execute(null));

		verify(ownershipQuery).putParameter(Bean.DOCUMENT_ID, "bean-1", false);
		verify(ownershipQuery).putParameter("attachment", "content-1", false);
		verify(dynamicReferenceQuery).putParameter("like", "%\":\"content-1\"%", false);
		assertTrue(RecordingContentManager.removedAttachments.contains("content-1"));
		assertTrue(RecordingContentManager.removedBeans.isEmpty());
	}

	/** Confirms that a content binding in another dynamic row protects an otherwise orphaned attachment. */
	@Test
	void executeKeepsOrphanCandidateWhenAnotherDynamicRowReferencesAttachment() {
		MetadataContext metadata = metadataContext(persistent("TEST_TABLE"));
		Attribute crossReference = mock(Attribute.class);
		when(crossReference.getAttributeType()).thenReturn(AttributeType.content);
		when(metadata.document().getPolymorphicAttribute(metadata.customer(), "otherAttachment"))
				.thenReturn(crossReference);
		SQL ownershipQuery = sqlReturningRows(Collections.emptyList());
		SQL dynamicReferenceQuery = dynamicReferenceQuery(List.<Object[]> of(new Object[] {
				"other-biz-id",
				"customer",
				"module",
				"document",
				"{\"otherAttachment\":\"content-1\"}"
		}));
		when(persistence.newSQL("select 1 from TEST_TABLE where bizId = :bizId and attachment = :attachment"))
				.thenReturn(ownershipQuery);
		when(persistence.newSQL("select bizId, bizCustomer, moduleName, documentName, fields from DYN_ENTITY where fields like :like"))
				.thenReturn(dynamicReferenceQuery);
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		RecordingContentManager.results = List.of(oldSearchResult("bean-1", "content-1", "attachment"));

		assertDoesNotThrow(() -> job.execute(null));

		assertTrue(RecordingContentManager.removedAttachments.isEmpty());
		verify(dynamicReferenceQuery).putParameter("like", "%\":\"content-1\"%", false);
	}

	/** Confirms that attachments on dynamic documents are resolved from the dynamic fields payload. */
	@Test
	@SuppressWarnings("boxing")
	void executeUsesDynamicEntityFieldsForDynamicDocumentAttachment() {
		MetadataContext metadata = metadataContext(persistent("IGNORED_TABLE"));
		when(metadata.document().isDynamic()).thenReturn(Boolean.TRUE);
		SQL ownershipQuery = sqlReturningRows(List.of(Integer.valueOf(1)));
		when(persistence.newSQL("select 1 from DYN_ENTITY where bizId = :bizId and fields like :attachment"))
				.thenReturn(ownershipQuery);
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		RecordingContentManager.results = List.of(oldSearchResult("bean-1", "content-1", "attachment"));

		assertDoesNotThrow(() -> job.execute(null));

		verify(ownershipQuery).putParameter(Bean.DOCUMENT_ID, "bean-1", false);
		verify(ownershipQuery).putParameter("attachment", "%\"attachment\":\"content-1\"%", false);
		assertTrue(RecordingContentManager.removedAttachments.isEmpty());
	}

	/** Confirms that a dynamic content attribute uses the dynamic fields payload on a static document. */
	@Test
	void executeUsesDynamicEntityFieldsForDynamicAttachmentAttribute() {
		MetadataContext metadata = metadataContext(persistent("IGNORED_TABLE"));
		Content dynamicContent = new Content();
		dynamicContent.setName("attachment");
		dynamicContent.setDynamic(true);
		when(metadata.document().getPolymorphicAttribute(metadata.customer(), "attachment"))
				.thenReturn(dynamicContent);
		SQL ownershipQuery = sqlReturningRows(List.of(Integer.valueOf(1)));
		when(persistence.newSQL("select 1 from DYN_ENTITY where bizId = :bizId and fields like :attachment"))
				.thenReturn(ownershipQuery);
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		RecordingContentManager.results = List.of(oldSearchResult("bean-1", "content-1", "attachment"));

		assertDoesNotThrow(() -> job.execute(null));

		verify(ownershipQuery).putParameter("attachment", "%\"attachment\":\"content-1\"%", false);
		assertTrue(RecordingContentManager.removedAttachments.isEmpty());
	}

	/** Confirms that a non-dynamic content attribute is checked through its physical table column. */
	@Test
	@SuppressWarnings("boxing")
	void executeUsesStaticColumnForNonDynamicAttachmentAttribute() {
		MetadataContext metadata = metadataContext(persistent("TEST_TABLE"));
		Content staticContent = new Content();
		staticContent.setName("attachment");
		staticContent.setDynamic(Boolean.FALSE);
		when(metadata.document().getPolymorphicAttribute(metadata.customer(), "attachment"))
				.thenReturn(staticContent);
		SQL ownershipQuery = sqlReturningRows(List.of(Integer.valueOf(1)));
		when(persistence.newSQL("select 1 from TEST_TABLE where bizId = :bizId and attachment = :attachment"))
				.thenReturn(ownershipQuery);
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		RecordingContentManager.results = List.of(oldSearchResult("bean-1", "content-1", "attachment"));

		assertDoesNotThrow(() -> job.execute(null));

		verify(ownershipQuery).putParameter("attachment", "content-1", false);
		assertTrue(RecordingContentManager.removedAttachments.isEmpty());
	}

	/** Confirms that a failed ownership lookup does not prevent later content from being assessed. */
	@Test
	void executeContinuesAfterOneContentLookupFails() {
		SQL sql = mock(SQL.class);
		when(persistence.newSQL(anyString())).thenReturn(sql);
		when(sql.scalarResults(Integer.class))
				.thenThrow(new IllegalStateException("lookup failed"))
				.thenReturn(Collections.emptyList());
		ProvidedRepositoryFactory.set(repositoryWithPersistent(persistent("TEST_TABLE")));
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		RecordingContentManager.results = List.of(oldSearchResult("bean-1", null, null),
													oldSearchResult("bean-2", null, null));

		assertDoesNotThrow(() -> job.execute(null));

		assertFalse(RecordingContentManager.removedBeans.contains("bean-1"));
		assertTrue(RecordingContentManager.removedBeans.contains("bean-2"));
		verify(persistence).commit(true);
	}

	/** Confirms that enabling content tracing preserves lookup-failure continuation semantics. */
	@Test
	void executeTracesContentLookupFailure() {
		SQL sql = mock(SQL.class);
		when(persistence.newSQL(anyString())).thenReturn(sql);
		when(sql.scalarResults(Integer.class)).thenThrow(new IllegalStateException("lookup failed"));
		ProvidedRepositoryFactory.set(repositoryWithPersistent(persistent("TEST_TABLE")));
		org.skyve.impl.util.UtilImpl.CONTENT_TRACE = true;
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		RecordingContentManager.results = List.of(oldSearchResult("bean-1", null, null));

		assertDoesNotThrow(() -> job.execute(null));

		assertTrue(RecordingContentManager.removedBeans.isEmpty());
		verify(persistence).commit(true);
	}

	/** Confirms that the safety limit stops discovery once both orphan collections are oversized. */
	@Test
	void executeSkipsDetectionWhenEachOrphanLimitHasAlreadyBeenExceeded() throws Exception {
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		setTrackingSet(job, "orphanedAttachmentContentIds", new OversizedEmptySet());
		setTrackingSet(job, "orphanedBeanBizIds", new OversizedEmptySet());
		RecordingContentManager.results = List.of(oldSearchResult("attachment-1", "content-1", "attachment"),
													oldSearchResult("bean-1", null, null));

		assertDoesNotThrow(() -> job.execute(null));

		assertTrue(RecordingContentManager.removedAttachments.isEmpty());
		assertTrue(RecordingContentManager.removedBeans.isEmpty());
		verify(persistence).commit(true);
	}

	public static class RecordingContentManager extends NoOpContentManager {
		private static List<SearchResult> results = Collections.emptyList();
		private static List<String> removedAttachments = new ArrayList<>();
		private static List<String> removedBeans = new ArrayList<>();
		private static boolean failRemovals;
		private static boolean failAll;

		static void reset() {
			results = Collections.emptyList();
			removedAttachments = new ArrayList<>();
			removedBeans = new ArrayList<>();
			failRemovals = false;
			failAll = false;
		}

		@Override
		public ContentIterable all() {
			if (failAll) {
				throw new IllegalStateException("content enumeration failed");
			}
			return () -> new ContentIterable.ContentIterator() {
				private final java.util.Iterator<SearchResult> delegate = results.iterator();

				@Override
				public long getTotalHits() {
					return 0;
				}

				@Override
				public boolean hasNext() {
					return delegate.hasNext();
				}

				@Override
				public SearchResult next() {
					return delegate.next();
				}
			};
		}

		@Override
		public void removeAttachment(String contentId) {
			if (failRemovals) {
				throw new IllegalStateException("attachment removal failed");
			}
			removedAttachments.add(contentId);
		}

		@Override
		public void removeBean(String bizId) {
			if (failRemovals) {
				throw new IllegalStateException("bean removal failed");
			}
			removedBeans.add(bizId);
		}
	}

	private static SearchResult searchResult(String bizId, String contentId, String attributeName) {
		SearchResult result = new SearchResult();
		result.setCustomerName("customer");
		result.setModuleName("module");
		result.setDocumentName("document");
		result.setBizId(bizId);
		result.setContentId(contentId);
		result.setAttributeName(attributeName);
		result.setLastModified(null);
		return result;
	}

	private static SearchResult oldSearchResult(String bizId, String contentId, String attributeName) {
		SearchResult result = searchResult(bizId, contentId, attributeName);
		result.setLastModified(new Date(0L));
		return result;
	}

	private static ProvidedRepository repositoryWithPersistent(Persistent persistent) {
		Document document = mock(Document.class);
		when(document.getPersistent()).thenReturn(persistent);
		return repositoryWithDocument(document);
	}

	private static ProvidedRepository repositoryWithDocument(Document document) {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(repository.getCustomer("customer")).thenReturn(customer);
		when(repository.getModule(customer, "module")).thenReturn(module);
		when(module.getDocument(customer, "document")).thenReturn(document);
		return repository;
	}

	private static Persistent persistent(String persistentIdentifier) {
		Persistent persistent = mock(Persistent.class);
		when(persistent.getPersistentIdentifier()).thenReturn(persistentIdentifier);
		return persistent;
	}

	private static SQL sqlReturningRows(List<Integer> rows) {
		SQL sql = mock(SQL.class);
		when(sql.scalarResults(Integer.class)).thenReturn(rows);
		return sql;
	}

	private static SQL dynamicReferenceQuery(List<Object[]> rows) {
		SQL sql = mock(SQL.class);
		when(sql.putParameter("like", "%\":\"content-1\"%", false)).thenReturn(sql);
		when(sql.tupleResults()).thenReturn(rows);
		return sql;
	}

	private static MetadataContext metadataContext(Persistent persistent) {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Module adminModule = mock(Module.class);
		Document dynamicEntity = mock(Document.class);
		Persistent dynamicPersistent = persistent("DYN_ENTITY");
		when(repository.getAllCustomerNames()).thenReturn(List.of());
		when(repository.getCustomer("customer")).thenReturn(customer);
		when(repository.getModule(customer, "module")).thenReturn(module);
		when(module.getDocument(customer, "document")).thenReturn(document);
		when(document.getPersistent()).thenReturn(persistent);
		when(customer.getModule(AppConstants.ADMIN_MODULE_NAME)).thenReturn(adminModule);
		when(adminModule.getDocument(customer, AppConstants.DYNAMIC_ENTITY_DOCUMENT_NAME)).thenReturn(dynamicEntity);
		when(dynamicEntity.getPersistent()).thenReturn(dynamicPersistent);
		ProvidedRepositoryFactory.set(repository);
		return new MetadataContext(repository, customer, module, document);
	}

	private record MetadataContext(ProvidedRepository repository,
								Customer customer,
								Module module,
								Document document) {
		// Test metadata bundle.
	}

	private static final class OversizedEmptySet extends AbstractSet<String> {
		@Override
		public Iterator<String> iterator() {
			return Collections.emptyIterator();
		}

		@Override
		public int size() {
			return 10001;
		}
	}

	private static void setTrackingSet(ContentGarbageCollectionJob job, String fieldName, Set<String> value) throws Exception {
		Field field = ContentGarbageCollectionJob.class.getDeclaredField(fieldName);
		field.setAccessible(true);
		field.set(job, value);
	}

	@SuppressWarnings("unchecked")
	private static Set<String> trackingSet(ContentGarbageCollectionJob job, String fieldName) throws Exception {
		Field field = ContentGarbageCollectionJob.class.getDeclaredField(fieldName);
		field.setAccessible(true);
		return (Set<String>) field.get(job);
	}

	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}
