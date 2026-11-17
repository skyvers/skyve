package org.skyve.metadata.module;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

/**
 * Tests for the default methods of {@link ModuleMember}.
 */
@SuppressWarnings("static-method")
class ModuleMemberTest {
	private static final ModuleMember ADMIN = () -> "admin";

	@AfterEach
	void clearPersistenceThreadLocal() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<?>) field.get(null)).remove();
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
		Customer customer = bindCurrentCustomer();
		Module module = mock(Module.class);
		when(customer.getModule("admin")).thenReturn(module);

		assertSame(module, ADMIN.toModule());
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
