package org.skyve.metadata.user;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
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
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.ModuleMemberTestSupport;

/**
 * Tests for the default methods of {@link ModuleRole}.
 */
@SuppressWarnings("static-method")
class ModuleRoleTest {
	/** A compile-time role reference as the generated per-module role enums implement it. */
	private enum TestRole implements ModuleRole {
		AUDIT_MANAGER("AuditManager"),
		MISSING("Missing");

		private final String roleName;

		private TestRole(String roleName) {
			this.roleName = roleName;
		}

		@Override
		public String moduleName() {
			return "admin";
		}

		@Override
		public String roleName() {
			return roleName;
		}
	}

	@AfterEach
	void clearPersistenceThreadLocal() throws Exception {
		ModuleMemberTestSupport.clearPersistenceThreadLocal();
	}

	@Test
	void assignedRoleNameIsTheModuleDotRoleName() {
		assertEquals("admin.AuditManager", TestRole.AUDIT_MANAGER.assignedRoleName());
	}

	@Test
	void toRoleWithCustomerResolvesTheRoleInTheModule() {
		Customer customer = mock(Customer.class);
		Role role = mock(Role.class);
		mockModule(customer, role);

		assertSame(role, TestRole.AUDIT_MANAGER.toRole(customer));
	}

	@Test
	void toRoleThrowsWhenTheModuleHasNoSuchRole() {
		Customer customer = mock(Customer.class);
		mockModule(customer, mock(Role.class));

		MetaDataException e = assertThrows(MetaDataException.class, () -> TestRole.MISSING.toRole(customer));
		assertEquals("Role Missing does not exist in module admin", e.getMessage());
	}

	/** A module keeps all its metadata by name, so the name can be another kind of metadata's. */
	@Test
	void toRoleThrowsWhenTheNameIsAJobsName() {
		Customer customer = mock(Customer.class);
		ModuleImpl module = new ModuleImpl();
		module.setName("admin");
		JobMetaDataImpl job = new JobMetaDataImpl();
		job.setName("Missing");
		module.putJob(job);
		when(customer.getModule("admin")).thenReturn(module);

		MetaDataException e = assertThrows(MetaDataException.class, () -> TestRole.MISSING.toRole(customer));
		assertEquals("Role Missing does not exist in module admin", e.getMessage());
	}

	@Test
	void toRoleResolvesThroughTheCurrentUsersCustomer() {
		Customer customer = ModuleMemberTestSupport.bindCurrentCustomer();
		Role role = mock(Role.class);
		mockModule(customer, role);

		assertAll(() -> assertSame(role, TestRole.AUDIT_MANAGER.toRole()),
					() -> assertEquals("Role Missing does not exist in module admin",
										assertThrows(MetaDataException.class, TestRole.MISSING::toRole).getMessage()));
	}

	/**
	 * Mocks the admin module, declaring only the AuditManager role, using the real
	 * {@link Module#getNullSafeRole(String)}.
	 */
	private static Module mockModule(Customer customer, Role auditManager) {
		Module module = mock(ModuleMemberTestSupport.ModuleDouble.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		when(module.getName()).thenReturn("admin");
		when(module.getRole("AuditManager")).thenReturn(auditManager);
		when(customer.getModule("admin")).thenReturn(module);
		return module;
	}
}
