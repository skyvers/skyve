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
 * Tests for the default methods of {@link ModuleSQL}.
 */
@SuppressWarnings("static-method")
class ModuleSQLTest {
	/** A compile-time SQL reference as the generated per-module SQL enums implement it. */
	private enum TestSQL implements ModuleSQL {
		S_CONTACTS("sContacts"),
		// declared in the module, but as a BizQL query
		B_CONTACTS("bContacts"),
		// declared in the module, but as a metadata query
		Q_CONTACTS("qContacts"),
		MISSING("sMissing");

		private final String queryName;

		private TestSQL(String queryName) {
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
	void toSQLWithCustomerResolvesTheSQLQueryInTheModule() {
		Customer customer = mock(Customer.class);
		SQLDefinition sql = mockModule(customer);

		assertSame(sql, TestSQL.S_CONTACTS.toSQL(customer));
	}

	@Test
	void toSQLThrowsWhenTheModuleHasNoSuchQuery() {
		Customer customer = mock(Customer.class);
		mockModule(customer);

		MetaDataException e = assertThrows(MetaDataException.class, () -> TestSQL.MISSING.toSQL(customer));
		assertEquals("SQL query sMissing does not exist in module admin", e.getMessage());
	}

	@Test
	void toSQLThrowsWhenTheNamedQueryIsNotSQL() {
		Customer customer = mock(Customer.class);
		mockModule(customer);

		assertAll(() -> assertThrows(MetaDataException.class, () -> TestSQL.B_CONTACTS.toSQL(customer)),
					() -> assertThrows(MetaDataException.class, () -> TestSQL.Q_CONTACTS.toSQL(customer)));
	}

	@Test
	void toSQLResolvesThroughTheCurrentUsersCustomer() {
		Customer customer = bindCurrentCustomer();
		SQLDefinition sql = mockModule(customer);

		assertSame(sql, TestSQL.S_CONTACTS.toSQL());
	}

	/**
	 * Mocks the admin module declaring one query of each kind and returns its SQL query.
	 */
	private static SQLDefinition mockModule(Customer customer) {
		MetaDataQueryDefinition qContacts = mock(MetaDataQueryDefinition.class);
		when(qContacts.getName()).thenReturn("qContacts");
		BizQLDefinition bContacts = mock(BizQLDefinition.class);
		when(bContacts.getName()).thenReturn("bContacts");
		SQLDefinition sContacts = mock(SQLDefinition.class);
		when(sContacts.getName()).thenReturn("sContacts");

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("admin");
		when(module.getMetadataQueries()).thenReturn(List.of(qContacts, bContacts, sContacts));
		when(customer.getModule("admin")).thenReturn(module);
		return sContacts;
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
