package org.skyve.metadata.model.document;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.util.function.Function;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

@SuppressWarnings("static-method")
class SingletonCachedBizletTest {
	private static class TestBean extends AbstractPersistentBean {
		private static final long serialVersionUID = 1L;
		private final String moduleName;
		private final String documentName;
		private String bizKey;

		private TestBean(String moduleName, String documentName, String bizId, String bizKey) {
			this.moduleName = moduleName;
			this.documentName = documentName;
			setBizId(bizId);
			this.bizKey = bizKey;
		}

		@Override
		public String getBizModule() {
			return moduleName;
		}

		@Override
		public String getBizDocument() {
			return documentName;
		}

		@Override
		public String getBizKey() {
			return bizKey;
		}

		@Override
		public void setBizKey(String bizKey) {
			this.bizKey = bizKey;
		}
	}

	private static class TestSingletonCachedBizlet extends SingletonCachedBizlet<TestBean> {
		// Intentionally empty.
	}

	private static void clearPersistenceThreadLocal() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			threadLocal.remove();
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}

	@AfterEach
	void cleanup() {
		SingletonCachedBizlet.dispose();
		clearPersistenceThreadLocal();
	}

	@Test
	void userScopeDoesNotCacheAndDelegatesToSingletonLookup() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("custA");
		when(user.getCustomer()).thenReturn(customer);
		when(user.getCustomerName()).thenReturn("custA");
		when(user.getScope("admin", "Contact")).thenReturn(DocumentPermissionScope.user);

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		DocumentQuery query = mock(DocumentQuery.class);
		when(persistence.newDocumentQuery("admin", "Contact")).thenReturn(query);

		TestBean persisted = new TestBean("admin", "Contact", "P1", "Persisted");
		persisted.setBizVersion(Integer.valueOf(1));
		when(query.beanResult()).thenReturn(persisted);

		TestBean result = new TestSingletonCachedBizlet().newInstance(new TestBean("admin", "Contact", "S1", "Seed"));
		assertSame(persisted, result);
	}

	@Test
	void globalScopeCachesFirstResultAndThenRetrievesByCachedBizId() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("custA");
		when(user.getCustomer()).thenReturn(customer);
		when(user.getCustomerName()).thenReturn("custA");
		when(user.getScope("admin", "Contact")).thenReturn(DocumentPermissionScope.global);

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		DocumentQuery query = mock(DocumentQuery.class);
		when(persistence.newDocumentQuery("admin", "Contact")).thenReturn(query);

		TestBean persisted = new TestBean("admin", "Contact", "P1", "Persisted");
		persisted.setBizVersion(Integer.valueOf(1));
		when(query.beanResult()).thenReturn(persisted);

		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);

		TestBean retrieved = new TestBean("admin", "Contact", "P1", "Retrieved");
		retrieved.setBizVersion(Integer.valueOf(1));
		when(persistence.retrieve(document, "P1")).thenReturn(retrieved);

		TestSingletonCachedBizlet bizlet = new TestSingletonCachedBizlet();
		TestBean first = bizlet.newInstance(new TestBean("admin", "Contact", "S1", "Seed1"));
		TestBean second = bizlet.newInstance(new TestBean("admin", "Contact", "S2", "Seed2"));

		assertSame(persisted, first);
		assertSame(retrieved, second);
	}

	@Test
	void staleCachedIdRemovesCacheWhenReplacementIsNotPersisted() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("custA");
		when(user.getCustomer()).thenReturn(customer);
		when(user.getCustomerName()).thenReturn("custA");
		when(user.getScope("admin", "Contact")).thenReturn(DocumentPermissionScope.global);

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		DocumentQuery query = mock(DocumentQuery.class);
		when(persistence.newDocumentQuery("admin", "Contact")).thenReturn(query);

		TestBean initialPersisted = new TestBean("admin", "Contact", "P1", "Initial");
		initialPersisted.setBizVersion(Integer.valueOf(1));
		TestBean replacementNotPersisted = new TestBean("admin", "Contact", "NP1", "Replacement");
		TestBean nextNoCacheLookup = new TestBean("admin", "Contact", "NP2", "Next");
		when(query.beanResult()).thenReturn(initialPersisted, replacementNotPersisted, nextNoCacheLookup);

		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(persistence.retrieve(document, "P1")).thenReturn(null);

		TestSingletonCachedBizlet bizlet = new TestSingletonCachedBizlet();
		bizlet.newInstance(new TestBean("admin", "Contact", "S1", "Seed1"));
		TestBean second = bizlet.newInstance(new TestBean("admin", "Contact", "S2", "Seed2"));
		TestBean third = bizlet.newInstance(new TestBean("admin", "Contact", "S3", "Seed3"));

		assertSame(replacementNotPersisted, second);
		assertSame(nextNoCacheLookup, third);
	}

	@Test
	void staleCachedIdIsReplacedWhenReplacementIsPersisted() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("custA");
		when(user.getCustomer()).thenReturn(customer);
		when(user.getCustomerName()).thenReturn("custA");
		when(user.getScope("admin", "Contact")).thenReturn(DocumentPermissionScope.global);

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		DocumentQuery query = mock(DocumentQuery.class);
		when(persistence.newDocumentQuery("admin", "Contact")).thenReturn(query);

		TestBean initialPersisted = new TestBean("admin", "Contact", "P1", "Initial");
		initialPersisted.setBizVersion(Integer.valueOf(1));
		TestBean replacementPersisted = new TestBean("admin", "Contact", "P2", "Replacement");
		replacementPersisted.setBizVersion(Integer.valueOf(1));
		when(query.beanResult()).thenReturn(initialPersisted, replacementPersisted);

		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);

		TestBean retrievedAfterReplace = new TestBean("admin", "Contact", "P2", "RetrievedAfterReplace");
		retrievedAfterReplace.setBizVersion(Integer.valueOf(1));
		when(persistence.retrieve(document, "P1")).thenReturn(null);
		when(persistence.retrieve(document, "P2")).thenReturn(retrievedAfterReplace);

		TestSingletonCachedBizlet bizlet = new TestSingletonCachedBizlet();
		bizlet.newInstance(new TestBean("admin", "Contact", "S1", "Seed1"));
		TestBean second = bizlet.newInstance(new TestBean("admin", "Contact", "S2", "Seed2"));
		TestBean third = bizlet.newInstance(new TestBean("admin", "Contact", "S3", "Seed3"));

		assertSame(replacementPersisted, second);
		assertSame(retrievedAfterReplace, third);
	}

	@Test
	void newInstanceWithScopeUsesWithDocumentPermissionScopes() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("custA");
		when(user.getCustomer()).thenReturn(customer);
		when(user.getCustomerName()).thenReturn("custA");
		when(user.getScope("admin", "Contact")).thenReturn(DocumentPermissionScope.global);

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		DocumentQuery query = mock(DocumentQuery.class);
		when(persistence.newDocumentQuery("admin", "Contact")).thenReturn(query);
		when(query.beanResult()).thenReturn(null);
		when(persistence.withDocumentPermissionScopes(eq(DocumentPermissionScope.global),
				org.mockito.ArgumentMatchers.<Function<Persistence, PersistentBean>>any()))
				.thenAnswer(invocation -> {
					Function<Persistence, PersistentBean> function = invocation.getArgument(1);
					return function.apply(persistence);
				});

		TestBean seed = new TestBean("admin", "Contact", "S1", "Seed");
		TestBean result = new TestSingletonCachedBizlet().newInstance(seed, DocumentPermissionScope.global);

		assertSame(seed, result);
		verify(persistence).withDocumentPermissionScopes(eq(DocumentPermissionScope.global),
				org.mockito.ArgumentMatchers.<Function<Persistence, PersistentBean>>any());
	}

	@Test
	void customerScopeUsesCustomerSpecificCacheKeyAndRetrievePath() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("custA");
		when(user.getCustomer()).thenReturn(customer);
		when(user.getCustomerName()).thenReturn("custA");
		when(user.getScope("admin", "Contact")).thenReturn(DocumentPermissionScope.customer);

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		DocumentQuery query = mock(DocumentQuery.class);
		when(persistence.newDocumentQuery("admin", "Contact")).thenReturn(query);

		TestBean persisted = new TestBean("admin", "Contact", "P1", "Persisted");
		persisted.setBizVersion(Integer.valueOf(1));
		when(query.beanResult()).thenReturn(persisted);

		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);

		TestBean retrieved = new TestBean("admin", "Contact", "P1", "Retrieved");
		retrieved.setBizVersion(Integer.valueOf(1));
		when(persistence.retrieve(document, "P1")).thenReturn(retrieved);

		TestSingletonCachedBizlet bizlet = new TestSingletonCachedBizlet();
		TestBean first = bizlet.newInstance(new TestBean("admin", "Contact", "S1", "Seed1"));
		TestBean second = bizlet.newInstance(new TestBean("admin", "Contact", "S2", "Seed2"));

		assertSame(persisted, first);
		assertSame(retrieved, second);
	}

	@Test
	void newInstanceWithScopeWrapsInnerExceptionAsDomainException() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("custA");
		when(user.getCustomer()).thenReturn(customer);
		when(user.getCustomerName()).thenReturn("custA");
		when(user.getScope("admin", "Contact")).thenReturn(DocumentPermissionScope.global);

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		DocumentQuery query = mock(DocumentQuery.class);
		when(persistence.newDocumentQuery("admin", "Contact")).thenReturn(query);
		when(query.beanResult()).thenThrow(new IllegalStateException("boom"));
		when(persistence.withDocumentPermissionScopes(eq(DocumentPermissionScope.global),
				org.mockito.ArgumentMatchers.<Function<Persistence, PersistentBean>>any()))
				.thenAnswer(invocation -> {
					Function<Persistence, PersistentBean> function = invocation.getArgument(1);
					return function.apply(persistence);
				});

		TestBean seed = new TestBean("admin", "Contact", "S1", "Seed");
		assertThrows(DomainException.class,
				() -> new TestSingletonCachedBizlet().newInstance(seed, DocumentPermissionScope.global));
	}

	@Test
	void clearRemovesCustomerAndGlobalKeysWithoutError() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("custA");
		when(user.getCustomer()).thenReturn(customer);

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		assertDoesNotThrow(() -> SingletonCachedBizlet.clear("admin", "Contact"));
	}
}
