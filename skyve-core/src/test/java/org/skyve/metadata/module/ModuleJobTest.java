package org.skyve.metadata.module;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryDefinitionImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;

/**
 * Tests for the default methods of {@link ModuleJob}.
 */
@SuppressWarnings("static-method")
class ModuleJobTest {
	/** A compile-time job reference as the generated per-module job enums implement it. */
	private enum TestJob implements ModuleJob {
		J_BULK_USER_CREATION;

		@Override
		public String moduleName() {
			return "admin";
		}

		@Override
		public String jobName() {
			return "jBulkUserCreation";
		}
	}

	@AfterEach
	void clearPersistenceThreadLocal() throws Exception {
		ModuleMemberTestSupport.clearPersistenceThreadLocal();
	}

	@Test
	void toJobWithCustomerResolvesTheJobInTheModule() {
		Customer customer = mock(Customer.class);
		JobMetaData job = mockJob(customer);

		assertSame(job, TestJob.J_BULK_USER_CREATION.toJob(customer));
	}

	@Test
	void toJobThrowsWhenTheModuleHasNoSuchJob() {
		Customer customer = mock(Customer.class);
		ModuleImpl module = new ModuleImpl();
		module.setName("admin");
		when(customer.getModule("admin")).thenReturn(module);

		MetaDataException e = assertThrows(MetaDataException.class, () -> TestJob.J_BULK_USER_CREATION.toJob(customer));
		assertEquals("Job admin.jBulkUserCreation is not defined in the skyve metadata.", e.getMessage());
	}

	/** A module keeps all its metadata by name, so the name can be another kind of metadata's. */
	@Test
	void toJobThrowsWhenTheNameIsAQuerysName() {
		Customer customer = mock(Customer.class);
		ModuleImpl module = new ModuleImpl();
		module.setName("admin");
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		query.setName("jBulkUserCreation");
		module.putQuery(query);
		when(customer.getModule("admin")).thenReturn(module);

		MetaDataException e = assertThrows(MetaDataException.class, () -> TestJob.J_BULK_USER_CREATION.toJob(customer));
		assertEquals("Job admin.jBulkUserCreation is not defined in the skyve metadata.", e.getMessage());
	}

	@Test
	void toJobResolvesThroughTheCurrentUsersCustomer() {
		Customer customer = ModuleMemberTestSupport.bindCurrentCustomer();
		JobMetaData job = mockJob(customer);

		assertSame(job, TestJob.J_BULK_USER_CREATION.toJob());
	}

	private static JobMetaData mockJob(Customer customer) {
		Module module = mock(Module.class);
		JobMetaData job = mock(JobMetaData.class);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getJob("jBulkUserCreation")).thenReturn(job);
		return job;
	}
}
