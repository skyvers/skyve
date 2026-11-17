package org.skyve.metadata.user;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;

/**
 * Tests for the default methods of {@link ModuleRole}.
 */
@SuppressWarnings("static-method")
class ModuleRoleTest {
	/** Lets Mockito's subclass mock maker call the interface's default methods for real. */
	private abstract static class ModuleDouble implements Module {
		private static final long serialVersionUID = 1L;
	}

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
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<?>) field.get(null)).remove();
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

	@Test
	void toRoleResolvesThroughTheCurrentUsersCustomer() {
		Customer customer = bindCurrentCustomer();
		Role role = mock(Role.class);
		mockModule(customer, role);

		assertAll(() -> assertSame(role, TestRole.AUDIT_MANAGER.toRole()),
					() -> assertThrows(MetaDataException.class, TestRole.MISSING::toRole));
	}

	/**
	 * Mocks the admin module, declaring only the AuditManager role, using the real
	 * {@link Module#getNullSafeRole(String)}.
	 */
	private static Module mockModule(Customer customer, Role auditManager) {
		Module module = mock(ModuleDouble.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		when(module.getName()).thenReturn("admin");
		when(module.getRole("AuditManager")).thenReturn(auditManager);
		when(customer.getModule("admin")).thenReturn(module);
		return module;
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
