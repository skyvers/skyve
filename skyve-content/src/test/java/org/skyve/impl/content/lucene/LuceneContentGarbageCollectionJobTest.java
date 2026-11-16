package org.skyve.impl.content.lucene;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentIterable.ContentIterator;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.job.ContentGarbageCollectionJob;
import org.skyve.impl.metadata.model.document.field.Content;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.SQL;

/**
 * Verifies garbage collection across the real Lucene index and filesystem content store.
 *
 * <p>The scenario indexes two old attachments, reports one as still owned by its persistent bean,
 * and reports the other as orphaned. After the production job runs, the referenced attachment must
 * remain retrievable while the orphan must be absent from both storage layers and the index count
 * must decrease. Metadata and database ownership are isolated with test doubles, but content
 * indexing, enumeration and removal use the production {@link LuceneContentManager} implementation.
 */
class LuceneContentGarbageCollectionJobTest {
	private Class<? extends AbstractContentManager> originalContentManagerClass;
	private ProvidedRepository originalRepository;
	private String originalContentDirectory;
	private boolean originalFileStorage;
	private Path tempContentDirectory;
	private AbstractPersistence persistence;
	@SuppressWarnings("resource") // Closed by teardown after the job shares its static Lucene lifecycle.
	private LuceneContentManager manager;

	@BeforeEach
	@SuppressWarnings("resource") // Ownership is retained by the test field and released in teardown.
	void setup() throws Exception {
		originalContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;
		originalRepository = ProvidedRepositoryFactory.get();
		originalContentDirectory = UtilImpl.CONTENT_DIRECTORY;
		originalFileStorage = UtilImpl.CONTENT_FILE_STORAGE;
		tempContentDirectory = Files.createTempDirectory("skyve-content-garbage-collection-");
		UtilImpl.CONTENT_DIRECTORY = tempContentDirectory.toString();
		UtilImpl.CONTENT_FILE_STORAGE = true;
		AbstractContentManager.IMPLEMENTATION_CLASS = LuceneContentManager.class;
		persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);
		configureMetadataAndQueries();
		manager = new LuceneContentManager();
		manager.startup();
	}

	@AfterEach
	void teardown() throws Exception {
		try {
			if (manager != null) {
				manager.shutdown();
			}
		}
		finally {
			AbstractContentManager.IMPLEMENTATION_CLASS = originalContentManagerClass;
			UtilImpl.CONTENT_DIRECTORY = originalContentDirectory;
			UtilImpl.CONTENT_FILE_STORAGE = originalFileStorage;
			if (originalRepository == null) {
				ProvidedRepositoryFactory.clear();
			}
			else {
				ProvidedRepositoryFactory.set(originalRepository);
			}
			unbindPersistenceFromThread();
			if (tempContentDirectory != null) {
				try (var paths = Files.walk(tempContentDirectory)) {
					paths.sorted(Comparator.reverseOrder()).forEach(path -> {
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
	}

	/**
	 * Confirms that one collection pass preserves referenced content and completely removes an orphan.
	 */
	@Test
	void executeRemovesOrphanedAttachmentFromIndexAndFileStoreButKeepsReferencedAttachment() throws Exception {
		String referencedContentId = "747be0d0-99bc-47c3-8a70-e65a8b465bec";
		String orphanedContentId = "d41ed9fb-0513-4232-b1e4-d0de7e09f12c";
		manager.put(attachment("referenced-biz-id", referencedContentId, new byte[] {1, 2, 3}), false);
		manager.put(attachment("orphaned-biz-id", orphanedContentId, new byte[] {4, 5, 6}), false);
		manager.close();
		assertEquals(2L, countHits());

		new ContentGarbageCollectionJob().execute(null);

		assertNotNull(manager.getAttachment(referencedContentId));
		assertNull(manager.getAttachment(orphanedContentId));
		assertEquals(1L, countHits());
		verify(persistence).commit(true);
	}

	private static AttachmentContent attachment(String bizId, String contentId, byte[] bytes) {
		AttachmentContent result = new AttachmentContent("customer",
													"module",
													"document",
													null,
													"user",
													bizId,
													"attachment")
													.attachment("test.txt", "text/plain", bytes);
		result.setContentId(contentId);
		result.setLastModified(new Date(0L));
		return result;
	}

	private void configureMetadataAndQueries() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Persistent persistent = persistent("TEST_TABLE");
		Module adminModule = mock(Module.class);
		Document dynamicEntity = mock(Document.class);
		Persistent dynamicPersistent = persistent("DYN_ENTITY");
		Content attachment = new Content();
		attachment.setName("attachment");
		when(repository.getAllCustomerNames()).thenReturn(List.of());
		when(repository.getCustomer("customer")).thenReturn(customer);
		when(repository.getModule(customer, "module")).thenReturn(module);
		when(module.getDocument(customer, "document")).thenReturn(document);
		when(document.getPersistent()).thenReturn(persistent);
		when(document.getPolymorphicAttribute(customer, "attachment")).thenReturn(attachment);
		when(customer.getModule("admin")).thenReturn(adminModule);
		when(adminModule.getDocument(customer, "DynamicEntity")).thenReturn(dynamicEntity);
		when(dynamicEntity.getPersistent()).thenReturn(dynamicPersistent);
		ProvidedRepositoryFactory.set(repository);

		when(persistence.newSQL(anyString())).thenAnswer(invocation -> {
			String statement = invocation.getArgument(0, String.class);
			if (statement.startsWith("select 1 from TEST_TABLE")) {
				return ownershipQuery();
			}
			if (statement.startsWith("select bizId, bizCustomer, moduleName, documentName, fields from DYN_ENTITY")) {
				SQL query = mock(SQL.class);
				when(query.putParameter(anyString(), anyString(), org.mockito.ArgumentMatchers.eq(false))).thenReturn(query);
				when(query.tupleResults()).thenReturn(List.of());
				return query;
			}
			throw new IllegalArgumentException("Unexpected SQL: " + statement);
		});
	}

	private static SQL ownershipQuery() {
		SQL query = mock(SQL.class);
		AtomicReference<String> bizId = new AtomicReference<>();
		when(query.putParameter(anyString(), anyString(), org.mockito.ArgumentMatchers.eq(false))).thenAnswer(invocation -> {
			if ("bizId".equals(invocation.getArgument(0, String.class))) {
				bizId.set(invocation.getArgument(1, String.class));
			}
			return query;
		});
		when(query.scalarResults(Integer.class)).thenAnswer(invocation ->
				"referenced-biz-id".equals(bizId.get()) ? List.of(Integer.valueOf(1)) : List.of());
		return query;
	}

	private static Persistent persistent(String persistentIdentifier) {
		Persistent persistent = mock(Persistent.class);
		when(persistent.getPersistentIdentifier()).thenReturn(persistentIdentifier);
		return persistent;
	}

	private long countHits() throws Exception {
		long result = 0L;
		ContentIterator iterator = manager.all().iterator();
		while (iterator.hasNext()) {
			iterator.next();
			result++;
		}
		return result;
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
