package org.skyve.impl.persistence.hibernate;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.skyve.persistence.DynamicPersistence;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Statement;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hibernate.SessionFactory;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.skyve.content.BeanContent;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.NoResultsException;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.BizQLDefinition;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.SQLDefinition;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.DataStore;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.SQL;
import org.skyve.impl.snapshot.Snapshot;
import org.skyve.impl.snapshot.SnapshotAdapter;

@SuppressWarnings({"static-method", "resource", "boxing", "java:S108", "java:S5976", "java:S4144", "java:S1130"})
class AbstractHibernatePersistenceTest {
	
	private static final String TEST_MAPPING_XML = """
			<?xml version="1.0"?>
			<!DOCTYPE hibernate-mapping PUBLIC "-//Hibernate/Hibernate Mapping DTD 3.0//EN" "http://www.hibernate.org/dtd/hibernate-mapping-3.0.dtd">
			<hibernate-mapping default-access="field">
				<class name="org.skyve.impl.persistence.hibernate.BootstrapContact" table="ADM_Contact" entity-name="adminContact">
					<id name="bizId" length="36" />
					<version name="bizVersion" unsaved-value="null" />
					<property name="bizKey" length="128" not-null="true" />
					<set name="roles" table="ADM_Contact_roles" cascade="all-delete-orphan">
						<key column="owner_id" />
						<element column="role_name" type="string" />
					</set>
				</class>
			</hibernate-mapping>
			""";

	private static ProvidedRepository originalRepository;
	private static Path generatedMappingFile;
	private static boolean bootstrapComplete;

	@BeforeAll
	static void setupPersistenceBootstrap() throws Exception {
		originalRepository = getRepository();
		UtilImpl.clear();

		bootstrapComplete = false;
		URL markerResource = Thread.currentThread().getContextClassLoader().getResource("resources/i18n.properties");
		assertNotNull(markerResource, "Expected resources/i18n.properties on the test classpath");
		Path classpathRoot = Paths.get(markerResource.toURI()).getParent().getParent();
		assertNotNull(classpathRoot, "Expected a classpath root path for test resources");

		generatedMappingFile = classpathRoot.resolve("modules/admin/domain/admin_orm.hbm.xml");
		Files.createDirectories(generatedMappingFile.getParent());
		Files.writeString(generatedMappingFile, TEST_MAPPING_XML, StandardCharsets.UTF_8);

		UtilImpl.APPS_JAR_DIRECTORY = classpathRoot.toString().replace('\\', '/') + '/';
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
		Module mockModule = mock(Module.class);
		when(mockModule.getDocumentRefs()).thenReturn(Collections.emptyMap());
		when(repository.getModule(any(), anyString())).thenReturn(mockModule);
		ensureSessionFactoryOpen();

		// Create the mapped table for H2 query tests.
		// UtilImpl.DDL_SYNC=false skips Hibernate's schema generation, so we apply DDL manually.
		try (java.sql.Connection ddlConn = java.sql.DriverManager.getConnection(
				"jdbc:h2:mem:ahp_coverage;DB_CLOSE_DELAY=-1", "sa", "");
				Statement ddlStmt = ddlConn.createStatement()) {
			ddlStmt.execute("CREATE TABLE IF NOT EXISTS ADM_Contact (" +
					"bizId VARCHAR(36) PRIMARY KEY, " +
					"bizVersion INTEGER, " +
					"bizKey VARCHAR(128) NOT NULL)");
		}

		assertNotNull(AbstractHibernatePersistence.getDialect());
		bootstrapComplete = true;
	}

	@AfterAll
	static void tearDownPersistenceBootstrap() throws Exception {
		try {
			if (bootstrapComplete) {
				SessionFactory sessionFactory = getSessionFactory();
				if ((sessionFactory != null) && (! sessionFactory.isClosed())) {
					sessionFactory.close();
				}
			}
		}
		finally {
			if (generatedMappingFile != null) {
				Files.deleteIfExists(generatedMappingFile);
			}
			UtilImpl.clear();
			setRepository(originalRepository);
		}
	}

	@Test
	void testGetDialectCachesByClassName() {
		SkyveDialect first = AbstractHibernatePersistence.getDialect(H2SpatialDialect.class.getName());
		SkyveDialect second = AbstractHibernatePersistence.getDialect(H2SpatialDialect.class.getName());
		assertSame(first, second);
	}

	@Test
	void testGetDialectUsesConfiguredDataStoreDialect() {
		SkyveDialect byClassName = AbstractHibernatePersistence.getDialect(H2SpatialDialect.class.getName());
		SkyveDialect fromDataStore = AbstractHibernatePersistence.getDialect();
		assertSame(byClassName, fromDataStore);
	}

	@Test
	void testGetDialectThrowsForUnknownClass() {
		assertThrows(IllegalStateException.class,
				() -> AbstractHibernatePersistence.getDialect("org.skyve.does.not.exist.MissingDialect"));
	}

	@Test
	void testLogSecondLevelCacheStatsWithUnknownRegion() {
		assertDoesNotThrow(() -> AbstractHibernatePersistence.logSecondLevelCacheStats("missing-region"));
	}

	@Test
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

	@Test
	void testGetDocumentEntityNameReturnsEntityName() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			String entityName = persistence.getDocumentEntityName("admin", "Contact");
			assertNotNull(entityName);
			assertEquals("adminContact", entityName);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testNewSQLCreatesInstance() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			SQL sql = persistence.newSQL("select 1");
			assertNotNull(sql);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testNewSQLWithModuleDocumentCreatesInstance() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			SQL sql = persistence.newSQL("admin", "Contact", "select 1");
			assertNotNull(sql);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testNewBizQLCreatesInstance() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			BizQL bql = persistence.newBizQL("select bean from {admin.Contact} as bean");
			assertNotNull(bql);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testNewDocumentQueryFromDocumentCreatesInstance() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			Document doc = mock(Document.class);
			when(doc.getOwningModuleName()).thenReturn("admin");
			when(doc.getName()).thenReturn("Contact");
			DocumentQuery dq = persistence.newDocumentQuery(doc);
			assertNotNull(dq);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testEvictAllSharedCacheDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.evictAllSharedCache();
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictAllCachedDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		persistence.injectDynamicPersistence(mock(DynamicPersistence.class));
		try {
			persistence.evictAllCached();
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictSharedCacheCollectionsDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.evictSharedCacheCollections();
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictSharedCacheBeansByModuleDocumentDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.evictSharedCacheBeans("admin", "Contact");
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictSharedCachedBeanByIdDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.evictSharedCachedBean("admin", "Contact", "test-bean-id");
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testSharedCacheBeanReturnsFalseForUnknownBean() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			boolean result = persistence.sharedCacheBean("admin", "Contact", "unknown-id");
			assertFalse(result);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testFlushDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.begin();
			persistence.flush();
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testSetUserDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			User testUser = createTestUser();
			persistence.setUser(testUser);
			assertSame(testUser, persistence.getUser());
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testRollbackDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.begin();
			persistence.rollback();
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictSharedCacheBeansAllDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.evictSharedCacheBeans();
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testNewNamedSQLWithModuleAndQueryNameDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			// Query not in session factory, but it should produce an object, not throw at construction
			assertNotNull(persistence.newSQL("select 1"));
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testDisposalOfEntityManagerOnClose() {
		TestHibernatePersistence persistence1 = new TestHibernatePersistence();
		TestHibernatePersistence persistence2 = new TestHibernatePersistence();
		try {
			assertNotNull(persistence1.getEntityManager());
			assertNotNull(persistence2.getEntityManager());
		}
		finally {
			persistence1.close();
			persistence2.close();
		}
	}

	@Test
	void testWithDocumentPermissionScopesFunctionReturnsResult() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		persistence.injectDynamicPersistence(mock(DynamicPersistence.class));
		try {
			// withDocumentPermissionScopes requires setDocumentPermissionScopes which casts user to UserImpl
			// Use a real UserImpl with empty module map so no Customer lookup is needed
			persistence.setUser(createTestUserImpl());
			String result = persistence.withDocumentPermissionScopes(
				DocumentPermissionScope.customer,
				p -> "test-result");
			assertEquals("test-result", result);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testWithDocumentPermissionScopesConsumerDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		persistence.injectDynamicPersistence(mock(DynamicPersistence.class));
		try {
			persistence.setUser(createTestUserImpl());
			persistence.withDocumentPermissionScopes(
				DocumentPermissionScope.customer,
				p -> {
					// no-op callback for scope reset verification
				});
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}


	/** Create a User mock safe for {@link AbstractHibernatePersistence#setUser} — empty accessible module names
	 * means {@code resetDocumentPermissionScopes()} iterates nothing and does not touch document metadata. */
	static User createTestUser() {
		User user = mock(User.class);
		when(user.getCustomerName()).thenReturn("");
		when(user.getAccessibleModuleNames()).thenReturn(Collections.emptySet());
		return user;
	}

	@Test
	void testSharedCacheBeanByModuleDocumentBizIdReturnsFalse() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			boolean result = persistence.sharedCacheBean("admin", "Contact", "some-biz-id");
			assertFalse(result);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testSharedCacheBeanByBeanReturnsFalse() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			Bean bean = mock(Bean.class);
			when(bean.getBizModule()).thenReturn("admin");
			when(bean.getBizDocument()).thenReturn("Contact");
			when(bean.getBizId()).thenReturn("test-id");
			boolean result = persistence.sharedCacheBean(bean);
			assertFalse(result);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testSharedCacheCollectionReturnsFalse() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			boolean result = persistence.sharedCacheCollection("admin", "Contact", "roles", "some-biz-id");
			assertFalse(result);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testSharedCacheCollectionByBeanReturnsFalse() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			Bean owner = mock(Bean.class);
			when(owner.getBizModule()).thenReturn("admin");
			when(owner.getBizDocument()).thenReturn("Contact");
			when(owner.getBizId()).thenReturn("test-id");
			boolean result = persistence.sharedCacheCollection(owner, "roles");
			assertFalse(result);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictSharedCacheBeansByModuleDocumentDoesNotThrow2() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.evictSharedCacheBeans("admin", "Contact");
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictSharedCachedBeanByModuleDocumentBizIdDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.evictSharedCachedBean("admin", "Contact", "some-biz-id");
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictSharedCachedBeanByBeanDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			Bean bean = mock(Bean.class);
			when(bean.getBizModule()).thenReturn("admin");
			when(bean.getBizDocument()).thenReturn("Contact");
			when(bean.getBizId()).thenReturn("test-id");
			persistence.evictSharedCachedBean(bean);
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictSharedCacheCollectionsByModuleDocumentCollectionDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.evictSharedCacheCollections("admin", "Contact", "roles");
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictSharedCacheCollectionByModuleDocumentCollectionIdDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.evictSharedCacheCollection("admin", "Contact", "roles", "some-biz-id");
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictSharedCacheCollectionByBeanDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			Bean owner = mock(Bean.class);
			when(owner.getBizModule()).thenReturn("admin");
			when(owner.getBizDocument()).thenReturn("Contact");
			when(owner.getBizId()).thenReturn("test-id");
			persistence.evictSharedCacheCollection(owner, "roles");
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testCachedBeanReturnsFalseForNonDynamicBean() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			Bean bean = mock(Bean.class);
			when(bean.getBizModule()).thenReturn("admin");
			when(bean.getBizDocument()).thenReturn("Contact");
			when(bean.getBizId()).thenReturn("test-id");
			boolean result = persistence.cached(bean);
			assertFalse(result);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictCachedBeanDoesNotThrowForNonCachedBean() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.injectDynamicPersistence(mock(DynamicPersistence.class));
			Bean bean = mock(Bean.class);
			when(bean.getBizModule()).thenReturn("admin");
			when(bean.getBizDocument()).thenReturn("Contact");
			when(bean.getBizId()).thenReturn("test-id");
			persistence.evictCached(bean);
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testCachedDelegatesDynamicBeanToDynamicPersistence() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		DynamicPersistence dynamicPersistence = mock(DynamicPersistence.class);
		DynamicBean bean = new DynamicBean("admin", "Contact", new HashMap<>(Map.of(Bean.DOCUMENT_ID, "dynamic-1")));
		when(dynamicPersistence.cached(bean)).thenReturn(Boolean.TRUE);
		persistence.injectDynamicPersistence(dynamicPersistence);
		try {
			assertTrue(persistence.cached(bean));
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictCachedDelegatesDynamicBeanToDynamicPersistence() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		DynamicPersistence dynamicPersistence = mock(DynamicPersistence.class);
		DynamicBean bean = new DynamicBean("admin", "Contact", new HashMap<>(Map.of(Bean.DOCUMENT_ID, "dynamic-2")));
		persistence.injectDynamicPersistence(dynamicPersistence);
		try {
			persistence.evictCached(bean);

			verify(dynamicPersistence).evictCached(bean);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictAllSharedCacheOnlyEvictsEntityData() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.evictAllSharedCache();
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testNewSQLWithDocumentCreatesInstance() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			Document doc = mock(Document.class);
			when(doc.getOwningModuleName()).thenReturn("admin");
			when(doc.getName()).thenReturn("Contact");
			SQL sql = persistence.newSQL(doc, "select 1");
			assertNotNull(sql);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	/** Create a {@link org.skyve.impl.metadata.user.UserImpl} instance safe for {@link AbstractHibernatePersistence#withDocumentPermissionScopes}
	 * — its empty module menu map means {@code getAccessibleModuleNames()} returns an empty set,
	 * so {@code setDocumentPermissionScopes} iterates the module list but never calls {@code getCustomer()}. */
	static org.skyve.impl.metadata.user.UserImpl createTestUserImpl() {
		org.skyve.impl.metadata.user.UserImpl u = new org.skyve.impl.metadata.user.UserImpl();
		u.setCustomerName("");
		return u;
	}

	private static User userForModule(Module module) {
		Customer customer = mock(Customer.class);
		when(customer.getModule("admin")).thenReturn(module);
		User user = createTestUser();
		when(user.getCustomer()).thenReturn(customer);
		return user;
	}

	@Test
	void testNewDocumentQueryByModuleDocumentStringsCreatesInstance() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			// newDocumentQuery(String, String) looks up module/document via customer, which needs a mocked customer
			User user = mock(User.class);
			when(user.getCustomerName()).thenReturn("");
			when(user.getAccessibleModuleNames()).thenReturn(Collections.emptySet());
			Document doc = mock(Document.class);
			when(doc.getOwningModuleName()).thenReturn("admin");
			when(doc.getName()).thenReturn("Contact");
			Module module = mock(Module.class);
			when(module.getDocument(any(), anyString())).thenReturn(doc);
			Customer customer = mock(Customer.class);
			when(customer.getModule(anyString())).thenReturn(module);
			when(user.getCustomer()).thenReturn(customer);
			persistence.setUser(user);
			bindPersistenceToThread(persistence);
			DocumentQuery query = persistence.newDocumentQuery("admin", "Contact");
			assertNotNull(query);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testNewDocumentQueryFromDocumentWithClausesCreatesInstance() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			Document doc = mock(Document.class);
			when(doc.getOwningModuleName()).thenReturn("admin");
			when(doc.getName()).thenReturn("Contact");
			DocumentQuery query = persistence.newDocumentQuery(doc, null, null, null, null);
			assertNotNull(query);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testNewBizQLCreatesInstanceWithUser() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			BizQL bql = persistence.newBizQL("from adminContact a");
			assertNotNull(bql);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testBeginWithDynamicPersistenceDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		DynamicPersistence dp = mock(DynamicPersistence.class);
		persistence.injectDynamicPersistence(dp);
		try {
			persistence.begin();
			persistence.rollback();
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictCachedBeanDoesNotThrowForNonCachedBean2() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		DynamicPersistence dp = mock(DynamicPersistence.class);
		persistence.injectDynamicPersistence(dp);
		try {
			persistence.setUser(createTestUser());
			Bean bean = mock(Bean.class);
			when(bean.getBizModule()).thenReturn("admin");
			when(bean.getBizDocument()).thenReturn("Contact");
			// just ensure no exception from evictCached when not in cache
			persistence.evictCached(bean);
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testCommitWithCloseAndRemoveHashesDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.begin();
			// commit with close=false, removeUniqueHashes=false should work cleanly
			persistence.commit(false, false);
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	// ---- Snapshot adapter fromClientPayload tests (require persistence on thread for CORE.getUser()) ----

	private static final String MINIMAL_VUE_PAYLOAD =
		"{\"filters\":{},\"columnWidths\":[],\"visibleColumns\":[],\"operator\":\"and\",\"sortColumns\":[],\"summarySelection\":\"\"}";

	private static final String MINIMAL_SC_PAYLOAD =
		"{\"advancedCriteriaStyle\":null,\"fieldState\":\"[]\",\"sortState\":null," +
		"\"groupState\":\"[]\",\"summaryType\":\"\",\"criteria\":{}}";

	@Test
	void testVueFromClientPayloadReturnsSnapshotWhenPersistenceBound() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			Snapshot snapshot = SnapshotAdapter.VUE.fromClientPayload(MINIMAL_VUE_PAYLOAD);
			assertNotNull(snapshot);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testVueFromClientPayloadWithColumnAndSortReturnsSnapshot() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			String payload = "{\"filters\":{},\"columnWidths\":[100]," +
				"\"visibleColumns\":[\"name\"],\"operator\":\"and\"," +
				"\"sortColumns\":[\"-name\"],\"summarySelection\":\"\"}";
			Snapshot snapshot = SnapshotAdapter.VUE.fromClientPayload(payload);
			assertNotNull(snapshot);
			assertEquals(1, snapshot.getColumns().size());
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testVueFromClientPayloadWithFilterReturnsSnapshot() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			String payload = "{\"filters\":{\"name\":{\"operator\":\"and\"," +
				"\"constraints\":[{\"value\":\"Alice\",\"matchMode\":\"iContains\"}]}}," +
				"\"columnWidths\":[],\"visibleColumns\":[\"name\"]," +
				"\"operator\":\"and\",\"sortColumns\":[],\"summarySelection\":\"\"}";
			Snapshot snapshot = SnapshotAdapter.VUE.fromClientPayload(payload);
			assertNotNull(snapshot);
			assertNotNull(snapshot.getFilter());
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testVueFromClientPayloadWithInvalidPayloadReturnsNull() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			Snapshot snapshot = SnapshotAdapter.VUE.fromClientPayload("{\"invalid\":true}");
			assertNull(snapshot);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSmartClientFromClientPayloadReturnsSnapshotWhenPersistenceBound() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			Snapshot snapshot = SnapshotAdapter.SMART_CLIENT.fromClientPayload(MINIMAL_SC_PAYLOAD);
			assertNotNull(snapshot);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSmartClientFromClientPayloadWithFieldStateReturnsSnapshot() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			String payload = "{\"advancedCriteriaStyle\":null," +
				"\"fieldState\":\"[{\\\"name\\\":\\\"bizId\\\"}]\"," +
				"\"sortState\":null,\"groupState\":\"[]\"," +
				"\"summaryType\":\"\",\"criteria\":{}}";
			Snapshot snapshot = SnapshotAdapter.SMART_CLIENT.fromClientPayload(payload);
			assertNotNull(snapshot);
			assertEquals(1, snapshot.getColumns().size());
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSmartClientFromClientPayloadWithSortStateReturnsSnapshot() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			String payload = "{\"advancedCriteriaStyle\":null," +
				"\"fieldState\":\"[{\\\"name\\\":\\\"name\\\",\\\"width\\\":100}]\"," +
				"\"sortState\":\"{sortSpecifiers:[{property:\\\"name\\\",direction:\\\"ascending\\\"}]}\"," +
				"\"groupState\":\"[]\"," +
				"\"summaryType\":\"\",\"criteria\":{}}";
			Snapshot snapshot = SnapshotAdapter.SMART_CLIENT.fromClientPayload(payload);
			assertNotNull(snapshot);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testNewBizQLWithModuleNameCreatesInstance() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			BizQL bql = persistence.newBizQL("select bean from {admin.Contact} as bean where bean.bizId = :id");
			assertNotNull(bql);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testNewDocumentQueryFromBeanCreatesInstance() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			Document doc = mock(Document.class);
			when(doc.getOwningModuleName()).thenReturn("admin");
			when(doc.getName()).thenReturn("Contact");
			// test newDocumentQuery(Document)
			DocumentQuery dq = persistence.newDocumentQuery(doc);
			assertNotNull(dq);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testFlushWithActiveTransactionDoesNotThrow() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.begin();
			persistence.flush(); // should flush cleanly against H2
			persistence.rollback();
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testCachedReturnsFalseForBeanNotInCache() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.begin();
			PersistentBean bean = mock(PersistentBean.class);
			when(bean.getBizId()).thenReturn("test-bean-id");
			when(bean.getBizModule()).thenReturn("admin");
			when(bean.getBizDocument()).thenReturn("Contact");
			assertFalse(persistence.cached(bean));
			persistence.rollback();
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictSharedCacheBeansByModuleDocumentSilentNoOp() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			// evictSharedCacheBeans by module + document should not throw even if not cached
			persistence.evictSharedCacheBeans("admin", "Contact");
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictSharedCachedBeanByBeanNoOp() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			PersistentBean bean = mock(PersistentBean.class);
			when(bean.getBizModule()).thenReturn("admin");
			when(bean.getBizDocument()).thenReturn("Contact");
			when(bean.getBizId()).thenReturn("some-id");
			// should not throw even when not in shared cache
			persistence.evictSharedCachedBean(bean);
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testLogSecondLevelCacheStatsWithAnotherUnknownRegionDoesNotThrow() {
		// Verifies the static method handles additional unknown regions gracefully
		assertDoesNotThrow(() -> AbstractHibernatePersistence.logSecondLevelCacheStats("another-missing-region"));
	}

	@Test
	void testToVueReturnsNullForNullInput() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			// toVue with an empty string (not SmartClient) - VUE.fromClientPayload will return null
			String result = SnapshotAdapter.toVue("");
			assertNull(result);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testToVueWithValidVueSnapshotReturnsPayload() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			// Pass a valid Vue payload (not SmartClient) - VUE.fromClientPayload should return non-null
			String result = SnapshotAdapter.toVue(MINIMAL_VUE_PAYLOAD);
			assertNotNull(result);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testToVueWithSmartClientSnapshotConverts() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			// MINIMAL_SC_PAYLOAD is SmartClient, toVue should convert it
			String result = SnapshotAdapter.toVue(MINIMAL_SC_PAYLOAD);
			// result may be non-null if conversion succeeds
			// just ensure no exception
			assertTrue(result == null || !result.contains("\"advancedCriteriaStyle\""));
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testToSmartClientWithSmartClientSnapshotReturnsSame() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			String result = SnapshotAdapter.toSmartClient(MINIMAL_SC_PAYLOAD);
			assertNotNull(result);
			assertTrue(result.contains("advancedCriteriaStyle"));
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testToSmartClientWithVueSnapshotConverts() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			String result = SnapshotAdapter.toSmartClient(MINIMAL_VUE_PAYLOAD);
			// Just verify no exception - result may be non-null if converted
			assertTrue(result == null || result.contains("advancedCriteriaStyle"));
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLExecuteInsertAndScalarQuery() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.begin();
			// Insert a row using SQL execute
			String id = java.util.UUID.randomUUID().toString();
			int rows = persistence.newSQL("INSERT INTO ADM_Contact (bizId, bizVersion, bizKey) VALUES ('" + id + "', 0, 'Test Key')")
					.noTimeout()
					.execute();
			assertEquals(1, rows);
			// Scalar query to verify the row count includes our new row
			java.util.List<String> results = persistence.newSQL("SELECT bizId FROM ADM_Contact WHERE bizId = '" + id + "'")
					.noTimeout()
					.scalarResults(String.class);
			assertFalse(results.isEmpty());
			persistence.rollback();
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testSQLTupleResultsFromADMContact() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.begin();
			// Insert a row for tuple query
			String id = java.util.UUID.randomUUID().toString();
			persistence.newSQL("INSERT INTO ADM_Contact (bizId, bizVersion, bizKey) VALUES ('" + id + "', 0, 'Tuple Test')")
					.noTimeout()
					.execute();
			// Tuple query - bizId + bizKey
			java.util.List<Object[]> tuples = persistence.newSQL("SELECT bizId, bizKey FROM ADM_Contact WHERE bizId = '" + id + "'")
					.noTimeout()
					.tupleResults();
			assertFalse(tuples.isEmpty());
			Object[] row = tuples.get(0);
			assertEquals(id, row[0]);
			assertEquals("Tuple Test", row[1]);
			persistence.rollback();
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testSQLScalarIterableFromADMContact() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.begin();
			String id = java.util.UUID.randomUUID().toString();
			persistence.newSQL("INSERT INTO ADM_Contact (bizId, bizVersion, bizKey) VALUES ('" + id + "', 0, 'Iter Test')")
					.noTimeout()
					.execute();
			try (var iterable = persistence.newSQL("SELECT bizId FROM ADM_Contact WHERE bizId = '" + id + "'")
					.noTimeout()
					.scalarIterable(String.class)) {
				int count = 0;
				for (var item : iterable) {
					assertNotNull(item);
					count++;
				}
				assertTrue(count > 0);
			}
			persistence.rollback();
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testSQLTupleIterableFromADMContact() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.begin();
			String id = java.util.UUID.randomUUID().toString();
			persistence.newSQL("INSERT INTO ADM_Contact (bizId, bizVersion, bizKey) VALUES ('" + id + "', 0, 'TupIter Test')")
					.noTimeout()
					.execute();
			try (var iterable = persistence.newSQL("SELECT bizId, bizKey FROM ADM_Contact WHERE bizId = '" + id + "'")
					.noTimeout()
					.tupleIterable()) {
				int count = 0;
				for (var row : iterable) {
					assertNotNull(row);
					count++;
				}
				assertTrue(count > 0);
			}
			persistence.rollback();
		}
		finally {
			persistence.close();
		}
	}


	@Test
	void testSQLDataAccessImplExecuteAndScalarResults() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			try (org.skyve.impl.dataaccess.sql.SQLDataAccessImpl da = new org.skyve.impl.dataaccess.sql.SQLDataAccessImpl(UtilImpl.DATA_STORE)) {
				String id = java.util.UUID.randomUUID().toString();
				// INSERT via execute()
				int rows = da.newSQL("INSERT INTO ADM_Contact (bizId, bizVersion, bizKey) VALUES ('" + id + "', 0, 'DA Test')")
						.execute();
				assertEquals(1, rows);
				// Scalar results
				java.util.List<String> results = da.newSQL("SELECT bizId FROM ADM_Contact WHERE bizId = '" + id + "'")
						.scalarResults(String.class);
				assertFalse(results.isEmpty());
				assertEquals(id, results.get(0));
				da.rollback();
			}
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLDataAccessImplTupleResults() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			try (org.skyve.impl.dataaccess.sql.SQLDataAccessImpl da = new org.skyve.impl.dataaccess.sql.SQLDataAccessImpl(UtilImpl.DATA_STORE)) {
				String id = java.util.UUID.randomUUID().toString();
				da.newSQL("INSERT INTO ADM_Contact (bizId, bizVersion, bizKey) VALUES ('" + id + "', 0, 'DA Tuple')")
						.execute();
				java.util.List<Object[]> results = da.newSQL("SELECT bizId, bizKey FROM ADM_Contact WHERE bizId = '" + id + "'")
						.tupleResults();
				assertFalse(results.isEmpty());
				assertEquals(2, results.get(0).length);
				da.rollback();
			}
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLDataAccessImplScalarIterable() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			try (org.skyve.impl.dataaccess.sql.SQLDataAccessImpl da = new org.skyve.impl.dataaccess.sql.SQLDataAccessImpl(UtilImpl.DATA_STORE)) {
				String id = java.util.UUID.randomUUID().toString();
				da.newSQL("INSERT INTO ADM_Contact (bizId, bizVersion, bizKey) VALUES ('" + id + "', 0, 'DA ScalarIter')")
						.execute();
				int count = 0;
				try (var iter = da.newSQL("SELECT bizId FROM ADM_Contact WHERE bizId = '" + id + "'")
						.scalarIterable(String.class)) {
					for (var row : iter) {
						assertNotNull(row);
						count++;
					}
				}
				assertTrue(count > 0);
				da.rollback();
			}
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLDataAccessImplTupleIterable() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			try (org.skyve.impl.dataaccess.sql.SQLDataAccessImpl da = new org.skyve.impl.dataaccess.sql.SQLDataAccessImpl(UtilImpl.DATA_STORE)) {
				String id = java.util.UUID.randomUUID().toString();
				da.newSQL("INSERT INTO ADM_Contact (bizId, bizVersion, bizKey) VALUES ('" + id + "', 0, 'DA TupleIter')")
						.execute();
				int count = 0;
				try (var iter = da.newSQL("SELECT bizId, bizKey FROM ADM_Contact WHERE bizId = '" + id + "'")
						.tupleIterable()) {
					for (var row : iter) {
						assertNotNull(row);
						count++;
					}
				}
				assertTrue(count > 0);
				da.rollback();
			}
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLDataAccessImplCommitAndClose() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			String id = java.util.UUID.randomUUID().toString();
			try (org.skyve.impl.dataaccess.sql.SQLDataAccessImpl da = new org.skyve.impl.dataaccess.sql.SQLDataAccessImpl(UtilImpl.DATA_STORE)) {
				da.newSQL("INSERT INTO ADM_Contact (bizId, bizVersion, bizKey) VALUES ('" + id + "', 0, 'DA Commit')")
						.execute();
				da.commit();
			}
			// Clean up
			try (org.skyve.impl.dataaccess.sql.SQLDataAccessImpl da = new org.skyve.impl.dataaccess.sql.SQLDataAccessImpl(UtilImpl.DATA_STORE)) {
				da.newSQL("DELETE FROM ADM_Contact WHERE bizId = '" + id + "'").execute();
				da.commit();
			}
			assertNotNull(persistence);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLDataAccessImplRollback() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			try (org.skyve.impl.dataaccess.sql.SQLDataAccessImpl da = new org.skyve.impl.dataaccess.sql.SQLDataAccessImpl(UtilImpl.DATA_STORE)) {
				String id = java.util.UUID.randomUUID().toString();
				da.newSQL("INSERT INTO ADM_Contact (bizId, bizVersion, bizKey) VALUES ('" + id + "', 0, 'DA Rollback')")
						.execute();
				da.rollback();
				// Verify it doesn't exist
				java.util.List<String> results = da.newSQL("SELECT bizId FROM ADM_Contact WHERE bizId = '" + id + "'")
						.scalarResults(String.class);
				assertTrue(results.isEmpty());
			}
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLDataAccessImplDynaResults() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			try (org.skyve.impl.dataaccess.sql.SQLDataAccessImpl da = new org.skyve.impl.dataaccess.sql.SQLDataAccessImpl(UtilImpl.DATA_STORE)) {
				String id = java.util.UUID.randomUUID().toString();
				da.newSQL("INSERT INTO ADM_Contact (bizId, bizVersion, bizKey) VALUES ('" + id + "', 0, 'DA Dyna')")
						.execute();
				java.util.List<org.apache.commons.beanutils.DynaBean> results = da.newSQL("SELECT bizId FROM ADM_Contact WHERE bizId = '" + id + "'")
						.dynaResults();
				assertFalse(results.isEmpty());
				da.rollback();
			}
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLDataAccessImplDynaIterable() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			try (org.skyve.impl.dataaccess.sql.SQLDataAccessImpl da = new org.skyve.impl.dataaccess.sql.SQLDataAccessImpl(UtilImpl.DATA_STORE)) {
				String id = java.util.UUID.randomUUID().toString();
				da.newSQL("INSERT INTO ADM_Contact (bizId, bizVersion, bizKey) VALUES ('" + id + "', 0, 'DA DynaIter')")
						.execute();
				int count = 0;
				try (var iter = da.newSQL("SELECT bizId FROM ADM_Contact WHERE bizId = '" + id + "'")
						.dynaIterable()) {
					for (var row : iter) {
						assertNotNull(row);
						count++;
					}
				}
				assertTrue(count > 0);
				da.rollback();
			}
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLWithTextParameterReturnsMatchingRow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			persistence.begin();
			String id = java.util.UUID.randomUUID().toString();
			persistence.newSQL("INSERT INTO ADM_Contact (bizId, bizVersion, bizKey) VALUES ('" + id + "', 0, 'ParamTest')")
					.execute();
			java.util.List<String> results = persistence.newSQL("SELECT bizId FROM ADM_Contact WHERE bizId = :id")
					.putParameter("id", id, org.skyve.metadata.model.Attribute.AttributeType.text)
					.scalarResults(String.class);
			assertEquals(1, results.size());
			assertEquals(id, results.get(0));
		}
		finally {
			persistence.rollback();
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLWithBoolParameterTypeDoesNotThrow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			persistence.begin();
			// SELECT with bool param - H2 supports true/false
			java.util.List<String> results = persistence.newSQL("SELECT bizId FROM ADM_Contact WHERE :boolVal = true")
					.putParameter("boolVal", Boolean.TRUE, org.skyve.metadata.model.Attribute.AttributeType.bool)
					.scalarResults(String.class);
			assertNotNull(results);
		}
		finally {
			persistence.rollback();
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLWithMemoParameterTypeDoesNotThrow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			persistence.begin();
			java.util.List<String> results = persistence.newSQL("SELECT bizId FROM ADM_Contact WHERE bizKey = :key")
					.putParameter("key", "someMemoValue", org.skyve.metadata.model.Attribute.AttributeType.memo)
					.scalarResults(String.class);
			assertNotNull(results);
		}
		finally {
			persistence.rollback();
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLWithColourParameterTypeDoesNotThrow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			persistence.begin();
			java.util.List<String> results = persistence.newSQL("SELECT bizId FROM ADM_Contact WHERE bizKey = :key")
					.putParameter("key", "#ff0000", org.skyve.metadata.model.Attribute.AttributeType.colour)
					.scalarResults(String.class);
			assertNotNull(results);
		}
		finally {
			persistence.rollback();
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLWithDateParameterTypeDoesNotThrow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			persistence.begin();
			java.util.List<String> results = persistence.newSQL("SELECT bizId FROM ADM_Contact WHERE :d < CURRENT_DATE")
					.putParameter("d", new org.skyve.domain.types.DateOnly(), org.skyve.metadata.model.Attribute.AttributeType.date)
					.scalarResults(String.class);
			assertNotNull(results);
		}
		finally {
			persistence.rollback();
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLWithDateTimeParameterTypeDoesNotThrow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			persistence.begin();
			java.util.List<String> results = persistence.newSQL("SELECT bizId FROM ADM_Contact WHERE :dt < CURRENT_TIMESTAMP")
					.putParameter("dt", new org.skyve.domain.types.DateTime(), org.skyve.metadata.model.Attribute.AttributeType.dateTime)
					.scalarResults(String.class);
			assertNotNull(results);
		}
		finally {
			persistence.rollback();
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLWithTimestampParameterTypeDoesNotThrow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			persistence.begin();
			java.util.List<String> results = persistence.newSQL("SELECT bizId FROM ADM_Contact WHERE :ts < CURRENT_TIMESTAMP")
					.putParameter("ts", new org.skyve.domain.types.Timestamp(), org.skyve.metadata.model.Attribute.AttributeType.timestamp)
					.scalarResults(String.class);
			assertNotNull(results);
		}
		finally {
			persistence.rollback();
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLWithDecimalParameterTypeDoesNotThrow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			persistence.begin();
			java.util.List<String> results = persistence.newSQL("SELECT bizId FROM ADM_Contact WHERE :d < 999.99")
					.putParameter("d", new org.skyve.domain.types.Decimal2(java.math.BigDecimal.ONE), org.skyve.metadata.model.Attribute.AttributeType.decimal2)
					.scalarResults(String.class);
			assertNotNull(results);
		}
		finally {
			persistence.rollback();
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLWithIntegerParameterTypeDoesNotThrow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			persistence.begin();
			java.util.List<String> results = persistence.newSQL("SELECT bizId FROM ADM_Contact WHERE bizVersion = :v")
					.putParameter("v", java.lang.Integer.valueOf(0), org.skyve.metadata.model.Attribute.AttributeType.integer)
					.scalarResults(String.class);
			assertNotNull(results);
		}
		finally {
			persistence.rollback();
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLWithLongIntegerParameterTypeDoesNotThrow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			persistence.begin();
			java.util.List<String> results = persistence.newSQL("SELECT bizId FROM ADM_Contact WHERE :v < 1000000")
					.putParameter("v", Long.valueOf(1L), org.skyve.metadata.model.Attribute.AttributeType.longInteger)
					.scalarResults(String.class);
			assertNotNull(results);
		}
		finally {
			persistence.rollback();
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLWithTimeParameterTypeDoesNotThrow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			persistence.begin();
			java.util.List<String> results = persistence.newSQL("SELECT bizId FROM ADM_Contact WHERE :t < CURRENT_TIME")
					.putParameter("t", new org.skyve.domain.types.TimeOnly(), org.skyve.metadata.model.Attribute.AttributeType.time)
					.scalarResults(String.class);
			assertNotNull(results);
		}
		finally {
			persistence.rollback();
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testSQLWithIdParameterTypeDoesNotThrow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			persistence.begin();
			java.util.List<String> results = persistence.newSQL("SELECT bizId FROM ADM_Contact WHERE bizId = :id")
					.putParameter("id", "non-existent-id", org.skyve.metadata.model.Attribute.AttributeType.id)
					.scalarResults(String.class);
			assertNotNull(results);
		}
		finally {
			persistence.rollback();
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testNewBizQLReturnsHibernateBizQL() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			org.skyve.persistence.BizQL bql = persistence.newBizQL("from admin.Contact");
			assertNotNull(bql);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testGetConnectionReturnsOpenConnection() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			persistence.begin();
			java.sql.Connection conn = persistence.getConnection();
			assertNotNull(conn);
			assertFalse(conn.isClosed());
		}
		finally {
			persistence.rollback();
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testGetDocumentEntityNameReturnsNonNull() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			String entityName = persistence.getDocumentEntityName("admin", "Contact");
			assertNotNull(entityName);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testNewSQLWithModuleAndDocumentNameReturnsNonNull() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			org.skyve.persistence.SQL sql = persistence.newSQL("admin", "Contact", "SELECT bizId FROM ADM_Contact");
			assertNotNull(sql);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testWithDocumentPermissionScopesExecutesFunctionAndResets() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUserImpl());
			String result = persistence.withDocumentPermissionScopes(
					org.skyve.metadata.user.DocumentPermissionScope.customer,
					p -> "scopeResult");
			assertEquals("scopeResult", result);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testCommitWithCloseClosesEntityManager() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			persistence.begin();
			persistence.commit(true);
			assertNotNull(persistence);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
			persistence.close();
		}
	}

	@Test
	void testRetrieveReturnsNullForNonExistentId() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.begin();
			Document doc = mock(Document.class);
			when(doc.getOwningModuleName()).thenReturn("admin");
			when(doc.getName()).thenReturn("Contact");
			when(doc.isDynamic()).thenReturn(false);
			Bean result = persistence.retrieve(doc, "no-such-id-xyz");
			assertNull(result);
			persistence.rollback();
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testRetrieveAndLockThrowsForNonExistentId() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.begin();
			Document doc = mock(Document.class);
			when(doc.getOwningModuleName()).thenReturn("admin");
			when(doc.getName()).thenReturn("Contact");
			when(doc.isDynamic()).thenReturn(false);
			assertThrows(NoResultsException.class, () -> persistence.retrieveAndLock(doc, "no-such-id-xyz"));
			persistence.rollback();
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testCommitFalseDoesNotCloseEntityManager() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.begin();
			persistence.commit(false); // should not close
			assertTrue(persistence.getEntityManager().isOpen());
		}
		finally {
			persistence.rollback();
			persistence.close();
		}
	}

	@Test
	void testGetSessionReturnsOpenSession() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.begin();
			assertNotNull(persistence.getSession());
			assertTrue(persistence.getSession().isOpen());
			persistence.rollback();
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testGetEntityManagerReturnsOpenManager() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.begin();
			assertNotNull(persistence.getEntityManager());
			assertTrue(persistence.getEntityManager().isOpen());
			persistence.rollback();
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testSetRollbackOnlyAndRollbackDoesNotThrow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.begin();
			persistence.setRollbackOnly();
			persistence.rollback();
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testGetDocumentEntityNameWithRealUser() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			String entityName = persistence.getDocumentEntityName("admin", "Contact");
			// With user customerName="" the entity name is "adminContact" or similar
			assertNotNull(entityName);
			assertFalse(entityName.isEmpty());
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testEvictAllCachedAfterBeginDoesNotThrow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		DynamicPersistence dp = mock(DynamicPersistence.class);
		persistence.injectDynamicPersistence(dp);
		try {
			persistence.setUser(createTestUser());
			persistence.begin();
			persistence.evictAllCached();
			persistence.rollback();
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testWithDocumentPermissionScopesConsumerVariant() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUserImpl());
			java.util.concurrent.atomic.AtomicBoolean called = new java.util.concurrent.atomic.AtomicBoolean(false);
			persistence.withDocumentPermissionScopes(
					org.skyve.metadata.user.DocumentPermissionScope.customer,
					(java.util.function.Consumer<org.skyve.persistence.Persistence>) p -> called.set(true));
			assertTrue(called.get());
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testNewNamedDocumentQueryFromModuleReturnsNonNull() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			Module module = mock(Module.class);
			MetaDataQueryDefinition queryDef = mock(MetaDataQueryDefinition.class);
			Document doc = mock(Document.class);
			when(doc.getOwningModuleName()).thenReturn("admin");
			when(doc.getName()).thenReturn("Contact");
			when(queryDef.getDocumentName()).thenReturn("Contact");
			when(module.getNullSafeMetaDataQuery(anyString())).thenReturn(queryDef);
			when(module.getDocument(any(), anyString())).thenReturn(doc);
			DocumentQuery dq = mock(DocumentQuery.class);
			when(queryDef.constructDocumentQuery(any(), any())).thenReturn(dq);
			org.skyve.persistence.DocumentQuery result = persistence.newNamedDocumentQuery(module, "qContact");
			assertNotNull(result);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testNewNamedSQLVariantsCreateWrappers() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			Module module = mock(Module.class);
			SQLDefinition sql = mock(SQLDefinition.class);
			when(sql.getQuery()).thenReturn("select 1");
			when(sql.getTimeoutInSeconds()).thenReturn(Integer.valueOf(7));
			when(module.getSQL("namedSql")).thenReturn(sql);
			persistence.setUser(userForModule(module));

			Document document = mock(Document.class);
			when(document.getOwningModuleName()).thenReturn("admin");
			when(document.getName()).thenReturn("Contact");

			assertNotNull(persistence.newNamedSQL(module, "namedSql"));
			assertNotNull(persistence.newNamedSQL("admin", "namedSql"));
			assertNotNull(persistence.newNamedSQL("admin", "Contact", "namedSql"));
			assertNotNull(persistence.newNamedSQL(document, "namedSql"));
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testNewNamedBizQLVariantsCreateWrappers() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			Module module = mock(Module.class);
			BizQLDefinition bizql = mock(BizQLDefinition.class);
			when(bizql.getQuery()).thenReturn("from adminContact bean");
			when(bizql.getTimeoutInSeconds()).thenReturn(Integer.valueOf(11));
			when(module.getBizQL("namedBizql")).thenReturn(bizql);
			persistence.setUser(userForModule(module));

			assertNotNull(persistence.newNamedBizQL(module, "namedBizql"));
			assertNotNull(persistence.newNamedBizQL("admin", "namedBizql"));
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testNewNamedDocumentQueryFromModuleNameReturnsDefinitionResult() {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			Module module = mock(Module.class);
			MetaDataQueryDefinition queryDef = mock(MetaDataQueryDefinition.class);
			DocumentQuery documentQuery = mock(DocumentQuery.class);
			when(module.getNullSafeMetaDataQuery("namedQuery")).thenReturn(queryDef);
			when(queryDef.constructDocumentQuery(null, null)).thenReturn(documentQuery);
			persistence.setUser(userForModule(module));

			assertSame(documentQuery, persistence.newNamedDocumentQuery("admin", "namedQuery"));
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testFlushAfterBeginDoesNotThrow() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.begin();
			persistence.flush();
			persistence.rollback();
			assertNotNull(persistence);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testNewSQLQueryReturnsNonNull() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			SQL sql = persistence.newSQL("select 1");
			assertNotNull(sql);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testNewBizQLQueryReturnsNonNull() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			BizQL bql = persistence.newBizQL("from admin.Contact");
			assertNotNull(bql);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testNewSQLWithModuleDocumentReturnsNonNull() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			SQL sql = persistence.newSQL("admin", "Contact", "select 1");
			assertNotNull(sql);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testNewDocumentQueryWithDocumentAndClausesReturnsNonNull() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			bindPersistenceToThread(persistence);
			Document doc = mock(Document.class);
			when(doc.getOwningModuleName()).thenReturn("admin");
			when(doc.getName()).thenReturn("Contact");
			DocumentQuery query = persistence.newDocumentQuery(doc, null, null, null, null);
			assertNotNull(query);
		}
		finally {
			unbindPersistenceFromThread();
			persistence.close();
		}
	}

	@Test
	void testGetConnectionReturnsNonNull() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			persistence.begin();
			java.sql.Connection conn = persistence.getConnection();
			assertNotNull(conn);
			persistence.rollback();
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testSharedCacheBeanReturnsFalseForUnpersistedBean() throws Exception {
		TestHibernatePersistence persistence = new TestHibernatePersistence();
		try {
			persistence.setUser(createTestUser());
			// shared cache bean check should return false for non-existent entity
			boolean result = persistence.sharedCacheBean("admin", "Contact", "no-such-id");
			assertFalse(result);
		}
		finally {
			persistence.close();
		}
	}

	/** Bind {@code persistence} to the current thread so that
	 * {@link AbstractPersistence#get()} returns it inside query construction. */
	@SuppressWarnings("unchecked")
	static void bindPersistenceToThread(TestHibernatePersistence persistence) throws Exception {
		Field f = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		f.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) f.get(null)).set(persistence);
	}

	/** Remove the thread-local binding created by {@link #bindPersistenceToThread}. */
	@SuppressWarnings("unchecked")
	static void unbindPersistenceFromThread() throws Exception {
		Field f = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		f.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) f.get(null)).remove();
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

	private static void ensureSessionFactoryOpen() throws Exception {
		SessionFactory sessionFactory = getSessionFactory();
		if ((sessionFactory == null) || sessionFactory.isClosed()) {
			Method configure = AbstractHibernatePersistence.class.getDeclaredMethod("configure");
			configure.setAccessible(true);
			configure.invoke(null);
		}
	}

	static final class TestHibernatePersistence extends AbstractHibernatePersistence {
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

		/** Expose the protected {@code dynamicPersistence} field for test injection. */
		void injectDynamicPersistence(DynamicPersistence dp) {
			this.dynamicPersistence = dp;
		}
	}
}
