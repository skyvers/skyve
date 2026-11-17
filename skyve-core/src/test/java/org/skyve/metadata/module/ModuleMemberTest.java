package org.skyve.metadata.module;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.customer.Customer;

/**
 * Tests for the default methods of {@link ModuleMember}.
 */
@SuppressWarnings("static-method")
class ModuleMemberTest {
	private static final ModuleMember ADMIN = () -> "admin";

	@AfterEach
	void clearPersistenceThreadLocal() throws Exception {
		ModuleMemberTestSupport.clearPersistenceThreadLocal();
	}

	@Test
	void toModuleWithCustomerResolvesTheModuleByName() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(customer.getModule("admin")).thenReturn(module);

		assertSame(module, ADMIN.toModule(customer));
	}

	@Test
	void toModuleResolvesThroughTheCurrentUsersCustomer() {
		Customer customer = ModuleMemberTestSupport.bindCurrentCustomer();
		Module module = mock(Module.class);
		when(customer.getModule("admin")).thenReturn(module);

		assertSame(module, ADMIN.toModule());
	}
}
