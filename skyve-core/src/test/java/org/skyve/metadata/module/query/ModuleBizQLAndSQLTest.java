package org.skyve.metadata.module.query;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.module.query.BizQLDefinitionImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryDefinitionImpl;
import org.skyve.impl.metadata.module.query.SQLDefinitionImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.ModuleMemberTestSupport;

/**
 * Tests for the default methods of {@link ModuleBizQL} and {@link ModuleSQL}, against the admin
 * module declaring one query of each kind: the metadata query qContacts, the BizQL query bContacts
 * and the SQL query sContacts.
 */
@SuppressWarnings("static-method")
class ModuleBizQLAndSQLTest {
	/** A compile-time BizQL reference as the generated per-module BizQL enums implement it. */
	private enum TestBizQL implements ModuleBizQL {
		B_CONTACTS("bContacts"),
		// declared in the module, but as a SQL query
		S_CONTACTS("sContacts"),
		// declared in the module, but as a metadata query
		Q_CONTACTS("qContacts"),
		MISSING("missing");

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

	/** A compile-time SQL reference as the generated per-module SQL enums implement it. */
	private enum TestSQL implements ModuleSQL {
		S_CONTACTS("sContacts"),
		// declared in the module, but as a BizQL query
		B_CONTACTS("bContacts"),
		// declared in the module, but as a metadata query
		Q_CONTACTS("qContacts"),
		MISSING("missing");

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
		ModuleMemberTestSupport.clearPersistenceThreadLocal();
	}

	@Test
	void toBizQLAndToSQLWithCustomerResolveTheirQueryInTheModule() {
		Customer customer = mock(Customer.class);
		ModuleImpl module = adminModule(customer);

		assertAll(() -> assertSame(module.getBizQL("bContacts"), TestBizQL.B_CONTACTS.toBizQL(customer)),
					() -> assertSame(module.getSQL("sContacts"), TestSQL.S_CONTACTS.toSQL(customer)));
	}

	@Test
	void toBizQLThrowsWhenTheModuleHasNoBizQLQueryOfTheName() {
		Customer customer = mock(Customer.class);
		adminModule(customer);

		assertAll(() -> assertEquals("BizQL query missing does not exist in module admin",
										assertThrows(MetaDataException.class, () -> TestBizQL.MISSING.toBizQL(customer)).getMessage()),
					() -> assertEquals("BizQL query sContacts does not exist in module admin",
										assertThrows(MetaDataException.class, () -> TestBizQL.S_CONTACTS.toBizQL(customer)).getMessage()),
					() -> assertEquals("BizQL query qContacts does not exist in module admin",
										assertThrows(MetaDataException.class, () -> TestBizQL.Q_CONTACTS.toBizQL(customer)).getMessage()));
	}

	@Test
	void toSQLThrowsWhenTheModuleHasNoSQLQueryOfTheName() {
		Customer customer = mock(Customer.class);
		adminModule(customer);

		assertAll(() -> assertEquals("SQL query missing does not exist in module admin",
										assertThrows(MetaDataException.class, () -> TestSQL.MISSING.toSQL(customer)).getMessage()),
					() -> assertEquals("SQL query bContacts does not exist in module admin",
										assertThrows(MetaDataException.class, () -> TestSQL.B_CONTACTS.toSQL(customer)).getMessage()),
					() -> assertEquals("SQL query qContacts does not exist in module admin",
										assertThrows(MetaDataException.class, () -> TestSQL.Q_CONTACTS.toSQL(customer)).getMessage()));
	}

	@Test
	void toBizQLAndToSQLResolveThroughTheCurrentUsersCustomer() {
		Customer customer = ModuleMemberTestSupport.bindCurrentCustomer();
		ModuleImpl module = adminModule(customer);

		assertAll(() -> assertSame(module.getBizQL("bContacts"), TestBizQL.B_CONTACTS.toBizQL()),
					() -> assertSame(module.getSQL("sContacts"), TestSQL.S_CONTACTS.toSQL()));
	}

	/**
	 * Makes the customer's admin module declare one query of each kind.
	 */
	private static ModuleImpl adminModule(Customer customer) {
		ModuleImpl result = new ModuleImpl();
		result.setName("admin");
		MetaDataQueryDefinitionImpl qContacts = new MetaDataQueryDefinitionImpl();
		qContacts.setName("qContacts");
		result.putQuery(qContacts);
		BizQLDefinitionImpl bContacts = new BizQLDefinitionImpl();
		bContacts.setName("bContacts");
		result.putQuery(bContacts);
		SQLDefinitionImpl sContacts = new SQLDefinitionImpl();
		sContacts.setName("sContacts");
		result.putQuery(sContacts);
		when(customer.getModule("admin")).thenReturn(result);
		return result;
	}
}
