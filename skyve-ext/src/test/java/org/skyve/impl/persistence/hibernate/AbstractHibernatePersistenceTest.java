package org.skyve.impl.persistence.hibernate;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.net.URL;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.hibernate.SessionFactory;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.skyve.content.BeanContent;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.DataStore;

class AbstractHibernatePersistenceTest {
	
	private static ProvidedRepository originalRepository;

	@BeforeAll
	static void setupPersistenceBootstrap() throws Exception {
		originalRepository = getRepository();
		UtilImpl.clear();

		URL modulesResource = Thread.currentThread().getContextClassLoader().getResource("modules");
		assertNotNull(modulesResource, "Expected test resource root to contain /modules");

		Path basePath = Paths.get(modulesResource.toURI()).getParent();
		assertNotNull(basePath, "Expected a parent path for /modules");

		UtilImpl.APPS_JAR_DIRECTORY = basePath.toString().replace('\\', '/') + '/';
		UtilImpl.DATA_STORE = new DataStore("org.h2.Driver",
												"jdbc:h2:mem:ahp_coverage;DB_CLOSE_DELAY=-1",
												"sa",
												"",
												H2SpatialDialect.class.getName());
		UtilImpl.DATA_STORES.put("coverage", UtilImpl.DATA_STORE);
		UtilImpl.DDL_SYNC = false;
		UtilImpl.SQL_TRACE = false;
		UtilImpl.QUERY_TRACE = false;
		UtilImpl.HIBERNATE_FAIL_ON_MISSING_CACHE = false;

		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.getAllVanillaModuleNames()).thenReturn(List.of(AppConstants.ADMIN_MODULE_NAME));
		when(repository.getAllCustomerNames()).thenReturn(List.of());
		setRepository(repository);

		assertNotNull(AbstractHibernatePersistence.getDialect());
	}

	@AfterAll
	static void tearDownPersistenceBootstrap() throws Exception {
		SessionFactory sessionFactory = getSessionFactory();
		if ((sessionFactory != null) && (! sessionFactory.isClosed())) {
			sessionFactory.close();
		}
		UtilImpl.clear();
		setRepository(originalRepository);
	}

	@SuppressWarnings("static-method")
	@Test
	void testGetDialectCachesByClassName() {
		SkyveDialect first = AbstractHibernatePersistence.getDialect(H2SpatialDialect.class.getName());
		SkyveDialect second = AbstractHibernatePersistence.getDialect(H2SpatialDialect.class.getName());
		assertSame(first, second);
	}

	@SuppressWarnings("static-method")
	@Test
	void testGetDialectUsesConfiguredDataStoreDialect() {
		SkyveDialect byClassName = AbstractHibernatePersistence.getDialect(H2SpatialDialect.class.getName());
		SkyveDialect fromDataStore = AbstractHibernatePersistence.getDialect();
		assertSame(byClassName, fromDataStore);
	}

	@SuppressWarnings("static-method")
	@Test
	void testGetDialectThrowsForUnknownClass() {
		assertThrows(IllegalStateException.class,
				() -> AbstractHibernatePersistence.getDialect("org.skyve.does.not.exist.MissingDialect"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testLogSecondLevelCacheStatsWithUnknownRegion() {
		AbstractHibernatePersistence.logSecondLevelCacheStats("missing-region");
	}

	@Test
	@SuppressWarnings("static-method")
	void testTransactionLifecycleWithoutDynamicPersistence() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			assertNotNull(persistence.getEntityManager());
			assertNotNull(persistence.getSession());

			persistence.begin();
			persistence.rollback();

			persistence.setRollbackOnly();
			persistence.begin();
			persistence.setRollbackOnly();
			persistence.commit(false, false);

			assertTrue(persistence.getEntityManager().isOpen());
		}
		finally {
			persistence.close();
			persistence.close();
		}
	}

	private static SessionFactory getSessionFactory() throws Exception {
		var sfField = AbstractHibernatePersistence.class.getDeclaredField("sf");
		sfField.setAccessible(true);
		return (SessionFactory) sfField.get(null);
	}

	private static ProvidedRepository getRepository() throws Exception {
		var repositoryField = ProvidedRepositoryFactory.class.getDeclaredField("repository");
		repositoryField.setAccessible(true);
		return (ProvidedRepository) repositoryField.get(null);
	}

	private static void setRepository(ProvidedRepository repository) throws Exception {
		var repositoryField = ProvidedRepositoryFactory.class.getDeclaredField("repository");
		repositoryField.setAccessible(true);
		repositoryField.set(null, repository);
	}

	private static final class TestHibernatePersistence extends AbstractHibernatePersistence {
		private static final long serialVersionUID = 3676907942783294478L;

		@Override
		protected void removeBeanContent(PersistentBean bean) {
			// no-op for unit tests
		}

		@Override
		protected void putBeanContent(BeanContent content) {
			// no-op for unit tests
		}

		@Override
		protected void closeContent() {
			// no-op for unit tests
		}
	}
}
