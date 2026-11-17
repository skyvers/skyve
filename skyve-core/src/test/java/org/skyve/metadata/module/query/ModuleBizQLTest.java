package org.skyve.metadata.module.query;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

/**
 * Tests for the default methods of {@link ModuleBizQL}.
 */
@SuppressWarnings("static-method")
class ModuleBizQLTest {
	/** A compile-time BizQL reference as the generated per-module BizQL enums implement it. */
	private enum TestBizQL implements ModuleBizQL {
		B_CONTACTS("bContacts"),
		// declared in the module, but as a SQL query
		S_CONTACTS("sContacts"),
		// declared in the module, but as a metadata query
		Q_CONTACTS("qContacts"),
		MISSING("bMissing");

		private final String queryName;

		private TestBizQL(String queryName) {
			this.queryName = queryName;
		}

		@Override
		public String moduleName() {
			return "admin";
		}

		@Override
		public String queryName() {
			return queryName;
		}
	}

	@AfterEach
	void clearPersistenceThreadLocal() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<?>) field.get(null)).remove();
	}

	@Test
	void toBizQLWithCustomerResolvesTheBizQLQueryInTheModule() {
		Customer customer = mock(Customer.class);
		BizQLDefinition bizQL = mockModule(customer);

		assertSame(bizQL, TestBizQL.B_CONTACTS.toBizQL(customer));
	}

	@Test
	void toBizQLThrowsWhenTheModuleHasNoSuchQuery() {
		Customer customer = mock(Customer.class);
		mockModule(customer);

		MetaDataException e = assertThrows(MetaDataException.class, () -> TestBizQL.MISSING.toBizQL(customer));
		assertEquals("BizQL query bMissing does not exist in module admin", e.getMessage());
	}

	@Test
	void toBizQLThrowsWhenTheNamedQueryIsNotBizQL() {
		Customer customer = mock(Customer.class);
		mockModule(customer);

		assertAll(() -> assertThrows(MetaDataException.class, () -> TestBizQL.S_CONTACTS.toBizQL(customer)),
					() -> assertThrows(MetaDataException.class, () -> TestBizQL.Q_CONTACTS.toBizQL(customer)));
	}

	@Test
	void toBizQLResolvesThroughTheCurrentUsersCustomer() {
		Customer customer = bindCurrentCustomer();
		BizQLDefinition bizQL = mockModule(customer);

		assertSame(bizQL, TestBizQL.B_CONTACTS.toBizQL());
	}

	/**
	 * Mocks the admin module declaring one query of each kind and returns its BizQL query.
	 */
	private static BizQLDefinition mockModule(Customer customer) {
		MetaDataQueryDefinition qContacts = mock(MetaDataQueryDefinition.class);
		when(qContacts.getName()).thenReturn("qContacts");
		SQLDefinition sContacts = mock(SQLDefinition.class);
		when(sContacts.getName()).thenReturn("sContacts");
		BizQLDefinition bContacts = mock(BizQLDefinition.class);
		when(bContacts.getName()).thenReturn("bContacts");

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("admin");
		when(module.getMetadataQueries()).thenReturn(List.of(qContacts, sContacts, bContacts));
		when(customer.getModule("admin")).thenReturn(module);
		return bContacts;
	}

	/**
	 * Binds a persistence to the thread whose user has the returned mocked customer.
	 */
	private static Customer bindCurrentCustomer() {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
		return customer;
	}
}
