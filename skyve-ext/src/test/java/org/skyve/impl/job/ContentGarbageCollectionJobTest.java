package org.skyve.impl.job;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.quartz.JobExecutionException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResult;
import org.skyve.domain.Bean;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.SQL;

class ContentGarbageCollectionJobTest {
	private Class<? extends AbstractContentManager> originalContentManagerClass;
	private ProvidedRepository originalRepository;
	private AbstractPersistence persistence;

	@BeforeEach
	void setup() throws Exception {
		originalContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;
		originalRepository = ProvidedRepositoryFactory.get();
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
		unbindPersistenceFromThread();
	}

	@Test
	void executeCommitsWhenThereIsNoContentToCollect() {
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();

		assertDoesNotThrow(() -> job.execute(null));
		verify(persistence).commit(true);
	}

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

	@Test
	void executeWrapsContentEnumerationFailureAndStillCommits() {
		ContentGarbageCollectionJob job = new ContentGarbageCollectionJob();
		RecordingContentManager.failAll = true;

		assertThrows(JobExecutionException.class, () -> job.execute(null));

		verify(persistence).commit(true);
	}

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

	private static void setTrackingSet(ContentGarbageCollectionJob job, String fieldName, TreeSet<String> value) throws Exception {
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
