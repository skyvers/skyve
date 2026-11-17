package org.skyve.metadata.module;

import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;

import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

/**
 * Supports tests of the compile-time metadata references ({@link ModuleMember} and the interfaces
 * that extend it), whose no-argument conversions resolve through the current user's customer.
 */
public final class ModuleMemberTestSupport {
	/** Lets Mockito's subclass mock maker call the interface's default methods for real. */
	public abstract static class ModuleDouble implements Module {
		private static final long serialVersionUID = 1L;
	}

	private ModuleMemberTestSupport() {
		// static helpers only
	}

	/**
	 * Binds a persistence to the thread whose user has the returned mocked customer. Call
	 * {@link #clearPersistenceThreadLocal()} after each test that calls this.
	 *
	 * @return	the current user's mocked customer
	 */
	public static Customer bindCurrentCustomer() {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
		return customer;
	}

	/**
	 * Removes the thread's persistence, which {@link #bindCurrentCustomer()} binds.
	 *
	 * @throws ReflectiveOperationException	if the thread local cannot be read.
	 */
	public static void clearPersistenceThreadLocal() throws ReflectiveOperationException {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<?>) field.get(null)).remove();
	}
}
