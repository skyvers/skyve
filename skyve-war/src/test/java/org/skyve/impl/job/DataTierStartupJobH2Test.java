package org.skyve.impl.job;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.content.BeanContent;
import org.skyve.domain.app.AppConstants;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.customer.ObserverMetaDataImpl;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.Observer;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;

import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpSession;
import modules.admin.Group.GroupExtension;
import modules.admin.domain.AdminDocument;
import modules.admin.domain.Group;
import modules.admin.domain.GroupRole;
import util.AbstractH2TestTruncate;

class DataTierStartupJobH2Test extends AbstractH2TestTruncate {
	private static final String CUSTOMER_NAME = "bizhub";

	private OriginalConfiguration originalConfiguration;
	private String bootstrapUserName;

	@BeforeEach
	void configureBootstrap() {
		originalConfiguration = new OriginalConfiguration(AbstractContentManager.IMPLEMENTATION_CLASS,
				UtilImpl.ENVIRONMENT_IDENTIFIER,
				UtilImpl.BOOTSTRAP_CUSTOMER,
				UtilImpl.BOOTSTRAP_USER,
				UtilImpl.BOOTSTRAP_EMAIL,
				UtilImpl.BOOTSTRAP_PASSWORD);

		bootstrapUserName = "bootstrap_" + UUID.randomUUID().toString().replace("-", "");
		UtilImpl.ENVIRONMENT_IDENTIFIER = "h2";
		UtilImpl.BOOTSTRAP_CUSTOMER = CUSTOMER_NAME;
		UtilImpl.BOOTSTRAP_USER = bootstrapUserName;
		UtilImpl.BOOTSTRAP_EMAIL = bootstrapUserName + "@example.test";
		UtilImpl.BOOTSTRAP_PASSWORD = "TestPassword0!";
		RecordingContentManager.reset();
		DataStartupObserver.reset(bootstrapUserName);
	}

	@AfterEach
	void restoreBootstrap() {
		AbstractContentManager.IMPLEMENTATION_CLASS = originalConfiguration.contentManagerClass();
		UtilImpl.ENVIRONMENT_IDENTIFIER = originalConfiguration.environmentIdentifier();
		UtilImpl.BOOTSTRAP_CUSTOMER = originalConfiguration.bootstrapCustomer();
		UtilImpl.BOOTSTRAP_USER = originalConfiguration.bootstrapUser();
		UtilImpl.BOOTSTRAP_EMAIL = originalConfiguration.bootstrapEmail();
		UtilImpl.BOOTSTRAP_PASSWORD = originalConfiguration.bootstrapPassword();
	}

	@Test
	void executeCreatesBootstrapUserAfterContentManagerStartup() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = RecordingContentManager.class;
		registerDataStartupObserver();
		CORE.getPersistence().commit(true);

		new DataTierStartupJob().execute(null);
		boolean bootstrapUserExists = bootstrapUserExists();

		assertTrue(RecordingContentManager.STARTED.get());
		assertTrue(RecordingContentManager.PUT_AFTER_STARTUP.get());
		assertTrue(bootstrapUserExists);
		assertTrue(DataStartupObserver.CALLED.get());
		assertTrue(DataStartupObserver.CONTENT_STARTED.get());
		assertTrue(DataStartupObserver.BOOTSTRAP_USER_VISIBLE.get());
	}

	@Test
	void executeDoesNotCreateBootstrapUserWhenContentManagerStartupFails() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = FailingContentManager.class;
		CORE.getPersistence().commit(true);

		new DataTierStartupJob().execute(null);
		boolean bootstrapUserExists = bootstrapUserExists();

		assertFalse(bootstrapUserExists);
		assertFalse(DataStartupObserver.CALLED.get());
	}

	@Test
	void executeUsesExistingBootstrapUserIdWhenObserverPersistsDocument() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = RecordingContentManager.class;
		registerDataStartupObserver();
		CORE.getPersistence().commit(true);

		new DataTierStartupJob().execute(null);
		DataStartupObserver.persistDocumentOnNextCallback();
		new DataTierStartupJob().execute(null);
		boolean bootstrapUserExists = bootstrapUserExists();

		assertTrue(bootstrapUserExists);
		assertNotNull(DataStartupObserver.PERSISTED_BIZ_USER_ID.get());
		assertEquals(DataStartupObserver.ACTIVE_USER_ID.get(), DataStartupObserver.PERSISTED_BIZ_USER_ID.get());
	}

	@Test
	void executeUsesConfiguredUserIdWhenBootstrapIsSkipped() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = RecordingContentManager.class;
		UtilImpl.ENVIRONMENT_IDENTIFIER = null;
		registerDataStartupObserver();
		DataStartupObserver.persistDocumentOnNextCallback();
		CORE.getPersistence().commit(true);

		new DataTierStartupJob().execute(null);
		boolean bootstrapUserExists = bootstrapUserExists();

		assertFalse(bootstrapUserExists);
		assertNotNull(DataStartupObserver.ACTIVE_USER_ID.get());
		assertEquals(7, UUID.fromString(DataStartupObserver.ACTIVE_USER_ID.get()).version());
		assertEquals(DataStartupObserver.ACTIVE_USER_ID.get(), DataStartupObserver.PERSISTED_BIZ_USER_ID.get());
	}

	private record OriginalConfiguration(Class<? extends AbstractContentManager> contentManagerClass,
			String environmentIdentifier,
			String bootstrapCustomer,
			String bootstrapUser,
			String bootstrapEmail,
			String bootstrapPassword) {
	}

	@SuppressWarnings({ "unused", "null" })
	private boolean bootstrapUserExists() {
		AbstractPersistence persistence = (AbstractPersistence) CORE.getPersistence();
		if (persistence.getUser() == null) {
			SuperUser user = new SuperUser();
			user.setCustomerName(CUSTOMER_NAME);
			user.setName("TestUser");
			user.setId("TestUser");
			persistence.setUser(user);
		}
		persistence.begin();

		DocumentQuery query = persistence.newDocumentQuery(AdminDocument.USER);
		query.getFilter().addEquals(AppConstants.USER_NAME_ATTRIBUTE_NAME, bootstrapUserName);
		return query.beanResult() != null;
	}

	private static void registerDataStartupObserver() {
		CustomerImpl customer = (CustomerImpl) ProvidedRepositoryFactory.getNullSafe().getCustomer(CUSTOMER_NAME);
		assertNotNull(customer);
		ObserverMetaDataImpl observer = new ObserverMetaDataImpl();
		observer.setClassName(DataStartupObserver.class.getName());
		customer.putObserver(observer);
	}

	public static class RecordingContentManager extends NoOpContentManager {
		private static final AtomicBoolean STARTED = new AtomicBoolean();
		private static final AtomicBoolean PUT_AFTER_STARTUP = new AtomicBoolean();

		static void reset() {
			STARTED.set(false);
			PUT_AFTER_STARTUP.set(false);
		}

		@Override
		public void startup() {
			STARTED.set(true);
		}

		@Override
		public void put(BeanContent content) {
			if (!STARTED.get()) {
				throw new IllegalStateException("Content was written before content manager startup");
			}
			PUT_AFTER_STARTUP.set(true);
		}
	}

	public static class FailingContentManager extends NoOpContentManager {
		@Override
		public void startup() {
			throw new IllegalStateException("startup failed");
		}
	}

	public static class DataStartupObserver implements Observer {
		private static final AtomicBoolean CALLED = new AtomicBoolean();
		private static final AtomicBoolean CONTENT_STARTED = new AtomicBoolean();
		private static final AtomicBoolean BOOTSTRAP_USER_VISIBLE = new AtomicBoolean();
		private static final AtomicBoolean PERSIST_DOCUMENT = new AtomicBoolean();
		private static final AtomicReference<String> EXPECTED_BOOTSTRAP_USER_NAME = new AtomicReference<>();
		private static final AtomicReference<String> ACTIVE_USER_ID = new AtomicReference<>();
		private static final AtomicReference<String> PERSISTED_BIZ_USER_ID = new AtomicReference<>();

		static void reset(String bootstrapUserName) {
			CALLED.set(false);
			CONTENT_STARTED.set(false);
			BOOTSTRAP_USER_VISIBLE.set(false);
			PERSIST_DOCUMENT.set(false);
			EXPECTED_BOOTSTRAP_USER_NAME.set(bootstrapUserName);
			ACTIVE_USER_ID.set(null);
			PERSISTED_BIZ_USER_ID.set(null);
		}

		static void persistDocumentOnNextCallback() {
			PERSIST_DOCUMENT.set(true);
		}

		@Override
		public void startup(@Nonnull Customer customer) {
			// Nothing to test for server startup here.
		}

		@Override
		public void dataTierReady(@Nonnull Customer customer) {
			CALLED.set(true);
			CONTENT_STARTED.set(RecordingContentManager.STARTED.get());

			DocumentQuery query = CORE.getPersistence()
					.newDocumentQuery(AdminDocument.USER);
			query.getFilter().addEquals(AppConstants.USER_NAME_ATTRIBUTE_NAME, EXPECTED_BOOTSTRAP_USER_NAME.get());
			BOOTSTRAP_USER_VISIBLE.set(query.beanResult() != null);

			if (PERSIST_DOCUMENT.compareAndSet(true, false)) {
				ACTIVE_USER_ID.set(CORE.getUser().getId());
				GroupExtension group = Group.newInstance();
				group.setName("Startup " + EXPECTED_BOOTSTRAP_USER_NAME.get().substring(0, 16));
				GroupRole role = GroupRole.newInstance();
				role.setRoleName("admin.BasicUser");
				group.addRolesElement(role);
				group = CORE.getPersistence().save(group);
				PERSISTED_BIZ_USER_ID.set(group.getBizUserId());
			}
		}

		@Override
		public void shutdown(@Nonnull Customer customer) {
			// Nothing to test for shutdown here.
		}

		@Override
		public void beforeBackup(@Nonnull Customer customer) {
			// Nothing to test for backup here.
		}

		@Override
		public void afterBackup(@Nonnull Customer customer) {
			// Nothing to test for backup here.
		}

		@Override
		public void beforeRestore(@Nonnull Customer customer) {
			// Nothing to test for restore here.
		}

		@Override
		public void afterRestore(@Nonnull Customer customer) {
			// Nothing to test for restore here.
		}

		@Override
		public void login(@Nonnull User user, @Nonnull HttpSession session) {
			// Nothing to test for login here.
		}

		@Override
		public void logout(@Nonnull User user, @Nonnull HttpSession session) {
			// Nothing to test for logout here.
		}
	}
}
