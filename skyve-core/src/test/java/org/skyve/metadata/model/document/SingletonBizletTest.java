package org.skyve.metadata.model.document;

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
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

@SuppressWarnings("static-method")
class SingletonBizletTest {
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

	private static class TestSingletonBizlet extends SingletonBizlet<TestBean> {
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
		clearPersistenceThreadLocal();
	}

	@Test
	void newInstanceReturnsExistingPersistentSingletonWhenQueryFindsOne() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		DocumentQuery query = mock(DocumentQuery.class);
		when(persistence.newDocumentQuery("admin", "Contact")).thenReturn(query);

		TestBean persisted = new TestBean("admin", "Contact", "P1", "Persisted");
		persisted.setBizVersion(Integer.valueOf(1));
		when(query.beanResult()).thenReturn(persisted);

		TestBean seed = new TestBean("admin", "Contact", "S1", "Seed");
		TestBean result = new TestSingletonBizlet().newInstance(seed);

		assertSame(persisted, result);
	}

	@Test
	void newInstanceFallsBackToSuperWhenNoPersistentSingletonExists() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);

		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		DocumentQuery query = mock(DocumentQuery.class);
		when(persistence.newDocumentQuery("admin", "Contact")).thenReturn(query);
		when(query.beanResult()).thenReturn(null);

		TestBean seed = new TestBean("admin", "Contact", "S1", "Seed");
		TestBean result = new TestSingletonBizlet().newInstance(seed);

		assertSame(seed, result);
	}

	@Test
	void newInstanceWithScopeUsesWithDocumentPermissionScopes() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);

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
		TestBean result = new TestSingletonBizlet().newInstance(seed, DocumentPermissionScope.global);

		assertSame(seed, result);
		verify(persistence).withDocumentPermissionScopes(eq(DocumentPermissionScope.global),
				org.mockito.ArgumentMatchers.<Function<Persistence, PersistentBean>>any());
	}

	@Test
	void newInstanceWithScopeWrapsInnerExceptionAsDomainException() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);

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
		TestSingletonBizlet bizlet = new TestSingletonBizlet();
		assertThrows(DomainException.class, () -> bizlet.newInstance(seed, DocumentPermissionScope.global));
	}
}
