package org.skyve.metadata.module.query;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.JobMetaDataImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.module.query.BizQLDefinitionImpl;
import org.skyve.impl.metadata.module.query.SQLDefinitionImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.ModuleMemberTestSupport.TestQuery;
import org.skyve.metadata.module.ModuleMemberTestSupport;

/**
 * Tests for the default methods of {@link ModuleQuery}.
 */
@SuppressWarnings("static-method")
class ModuleQueryTest {
	@AfterEach
	void clearPersistenceThreadLocal() throws Exception {
		ModuleMemberTestSupport.clearPersistenceThreadLocal();
	}

	@Test
	void toQueryWithCustomerResolvesTheMetaDataQueryInTheModule() {
		Customer customer = mock(Customer.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		mockModule(customer, query);

		assertSame(query, TestQuery.Q_CONTACTS.toQuery(customer));
	}

	@Test
	void toQueryThrowsWhenTheModuleHasNoSuchMetaDataQuery() {
		Customer customer = mock(Customer.class);
		mockModule(customer, mock(MetaDataQueryDefinition.class));

		MetaDataException e = assertThrows(MetaDataException.class, () -> TestQuery.MISSING.toQuery(customer));
		assertEquals("Query qMissing does not exist in module admin", e.getMessage());
	}

	/** A module keeps all its metadata by name, so the name can be another kind of metadata's. */
	@Test
	void toQueryThrowsWhenTheNameIsAJobsName() {
		Customer customer = mock(Customer.class);
		ModuleImpl module = new ModuleImpl();
		module.setName("admin");
		JobMetaDataImpl job = new JobMetaDataImpl();
		job.setName("qMissing");
		module.putJob(job);
		when(customer.getModule("admin")).thenReturn(module);

		MetaDataException e = assertThrows(MetaDataException.class, () -> TestQuery.MISSING.toQuery(customer));
		assertEquals("Query qMissing does not exist in module admin", e.getMessage());
	}

	/** The module's other kinds of query are not metadata queries. */
	@Test
	void toQueryThrowsWhenTheNameIsABizQLOrSQLQuerysName() {
		Customer customer = mock(Customer.class);
		ModuleImpl module = new ModuleImpl();
		module.setName("admin");
		BizQLDefinitionImpl bizQL = new BizQLDefinitionImpl();
		bizQL.setName("qContacts");
		module.putQuery(bizQL);
		SQLDefinitionImpl sql = new SQLDefinitionImpl();
		sql.setName("qMissing");
		module.putQuery(sql);
		when(customer.getModule("admin")).thenReturn(module);

		assertAll(() -> assertNull(module.getMetaDataQuery("qContacts")),
					() -> assertNull(module.getMetaDataQuery("qMissing")),
					() -> assertEquals("Query qContacts does not exist in module admin",
										assertThrows(MetaDataException.class, () -> TestQuery.Q_CONTACTS.toQuery(customer)).getMessage()),
					() -> assertEquals("Query qMissing does not exist in module admin",
										assertThrows(MetaDataException.class, () -> TestQuery.MISSING.toQuery(customer)).getMessage()));
	}

	@Test
	void toQueryResolvesThroughTheCurrentUsersCustomer() {
		Customer customer = ModuleMemberTestSupport.bindCurrentCustomer();
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		mockModule(customer, query);

		assertAll(() -> assertSame(query, TestQuery.Q_CONTACTS.toQuery()),
					() -> assertEquals("Query qMissing does not exist in module admin",
										assertThrows(MetaDataException.class, TestQuery.MISSING::toQuery).getMessage()));
	}

	/**
	 * Mocks the admin module, declaring only the qContacts metadata query, using the real
	 * {@link Module#getNullSafeMetaDataQuery(String)}.
	 */
	private static Module mockModule(Customer customer, MetaDataQueryDefinition qContacts) {
		Module module = mock(ModuleMemberTestSupport.ModuleDouble.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		when(module.getName()).thenReturn("admin");
		when(module.getMetaDataQuery("qContacts")).thenReturn(qContacts);
		when(customer.getModule("admin")).thenReturn(module);
		return module;
	}
}
