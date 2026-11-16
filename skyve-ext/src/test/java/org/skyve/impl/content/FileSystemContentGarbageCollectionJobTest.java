package org.skyve.impl.content;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Date;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.content.AttachmentContent;
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
 * Verifies garbage collection across the real file system content store.
 *
 * <p>The scenario stores two old attachments and one new one, reports one old attachment as still owned by its
 * persistent bean, and reports the others as orphaned. After the production job runs, the referenced attachment and
 * the new orphan must remain retrievable while the old orphan must be removed. Metadata and database ownership are
 * isolated with test doubles, but content storage, enumeration and removal use the production
 * {@link FileSystemContentManager} implementation.
 */
class FileSystemContentGarbageCollectionJobTest {
	private static final String REFERENCED_BIZ_ID = "referenced-biz-id";

	private Class<? extends AbstractContentManager> originalContentManagerClass;
	private ProvidedRepository originalRepository;
	private String originalContentDirectory;
	private boolean originalFileStorage;
	private AbstractPersistence persistence;

	@TempDir
	private Path tempDir;

	@BeforeEach
	void setup() throws Exception {
		originalContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;
		originalRepository = ProvidedRepositoryFactory.get();
		originalContentDirectory = UtilImpl.CONTENT_DIRECTORY;
		originalFileStorage = UtilImpl.CONTENT_FILE_STORAGE;
		UtilImpl.CONTENT_DIRECTORY = tempDir.toString() + File.separator;
		UtilImpl.CONTENT_FILE_STORAGE = true;
		AbstractContentManager.IMPLEMENTATION_CLASS = FileSystemContentManager.class;
		persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);
		configureMetadataAndQueries();
	}

	@AfterEach
	void teardown() throws Exception {
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
	}

	/**
	 * Confirms that one collection pass keeps referenced and recent content and removes an old orphan.
	 */
	@Test
	void executeRemovesOldOrphanedAttachmentButKeepsReferencedAndRecentAttachments() throws Exception {
		String referencedContentId = "747be0d0-99bc-47c3-8a70-e65a8b465bec";
		String orphanedContentId = "d41ed9fb-0513-4232-b1e4-d0de7e09f12c";
		String recentContentId = "0b8f6a52-3c1e-4d7a-9f20-5e6c1a2b3d4e";
		try (FileSystemContentManager manager = new FileSystemContentManager()) {
			manager.put(attachment(REFERENCED_BIZ_ID, referencedContentId, new Date(0L)), false);
			manager.put(attachment("orphaned-biz-id", orphanedContentId, new Date(0L)), false);
			manager.put(attachment("recent-biz-id", recentContentId, new Date()), false);
		}

		new ContentGarbageCollectionJob().execute(null);

		try (FileSystemContentManager manager = new FileSystemContentManager()) {
			assertNotNull(manager.getAttachment(referencedContentId));
			assertNull(manager.getAttachment(orphanedContentId));
			assertNotNull(manager.getAttachment(recentContentId));
		}
		verify(persistence).commit(true);
	}

	private static AttachmentContent attachment(String bizId, String contentId, Date lastModified) {
		AttachmentContent result = new AttachmentContent("customer",
													"module",
													"document",
													null,
													"user",
													bizId,
													"attachment")
													.attachment("test.txt", "text/plain", "test".getBytes(StandardCharsets.UTF_8));
		result.setContentId(contentId);
		result.setLastModified(lastModified);
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
				when(query.putParameter(anyString(), anyString(), eq(false))).thenReturn(query);
				when(query.tupleResults()).thenReturn(List.of());
				return query;
			}
			throw new IllegalArgumentException("Unexpected SQL: " + statement);
		});
	}

	private static SQL ownershipQuery() {
		SQL query = mock(SQL.class);
		AtomicReference<String> bizId = new AtomicReference<>();
		when(query.putParameter(anyString(), anyString(), eq(false))).thenAnswer(invocation -> {
			if ("bizId".equals(invocation.getArgument(0, String.class))) {
				bizId.set(invocation.getArgument(1, String.class));
			}
			return query;
		});
		when(query.scalarResults(Integer.class)).thenAnswer(invocation ->
				REFERENCED_BIZ_ID.equals(bizId.get()) ? List.of(Integer.valueOf(1)) : List.of());
		return query;
	}

	private static Persistent persistent(String persistentIdentifier) {
		Persistent persistent = mock(Persistent.class);
		when(persistent.getPersistentIdentifier()).thenReturn(persistentIdentifier);
		return persistent;
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
