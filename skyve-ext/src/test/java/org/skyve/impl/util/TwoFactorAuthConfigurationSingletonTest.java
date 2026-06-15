package org.skyve.impl.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.After;
import org.junit.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

@SuppressWarnings("static-method")
public class TwoFactorAuthConfigurationSingletonTest {
	@After
	public void after() {
		TwoFactorAuthConfigurationSingleton.getInstance().shutdown();
	}

	@Test
	public void testGetInstanceIsNotNull() {
		assertNotNull(TwoFactorAuthConfigurationSingleton.getInstance());
	}

	@Test
	public void testGetInstanceReturnsSameInstance() {
		assertSame(TwoFactorAuthConfigurationSingleton.getInstance(),
				TwoFactorAuthConfigurationSingleton.getInstance());
	}

	@Test
	public void testIsPushTfaStaticReturnsFalseForNull() {
		assertFalse(TwoFactorAuthConfigurationSingleton.isPushTfa((TwoFactorAuthCustomerConfiguration) null));
	}

	@Test
	public void testIsPushTfaStaticReturnsTrueForEmailType() {
		TwoFactorAuthCustomerConfiguration config =
				new TwoFactorAuthCustomerConfiguration("email", 60, "Subject", "Body");
		assertTrue(TwoFactorAuthConfigurationSingleton.isPushTfa(config));
	}

	@Test
	public void testIsPushTfaStaticReturnsFalseForOffType() {
		TwoFactorAuthCustomerConfiguration config =
				new TwoFactorAuthCustomerConfiguration("off", 0, "Subject", "Body");
		assertFalse(TwoFactorAuthConfigurationSingleton.isPushTfa(config));
	}

	@Test
	public void testIsPushTfaInstanceReturnsFalseForNullCustomerName() {
		assertFalse(TwoFactorAuthConfigurationSingleton.getInstance().isPushTfa((String) null));
	}

	@Test
	public void testIsPushTfaInstanceReturnsFalseForUnknownCustomer() {
		// No config loaded for this customer - getConfig returns null → isPushTfa returns false
		assertFalse(TwoFactorAuthConfigurationSingleton.getInstance().isPushTfa("unknownCustomer_xyzzy"));
	}

	@Test
	public void testGetConfigReturnsNullForUnknownCustomer() {
		assertNull(TwoFactorAuthConfigurationSingleton.getInstance().getConfig("nobody_xyzzy"));
	}

	@Test
	public void testClearConfigForAbsentKeyDoesNotThrow() {
		// Clearing a customer that was never added must be safe
		TwoFactorAuthConfigurationSingleton singleton = TwoFactorAuthConfigurationSingleton.getInstance();
		singleton.clearConfig("nonexistent_xyzzy");
		assertNotNull(singleton);
	}

	@Test
	public void testAddCachesConfigurationForCurrentCustomer() throws Exception {
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("test-customer");
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);

		TwoFactorAuthCustomerConfiguration config =
				new TwoFactorAuthCustomerConfiguration("email", 90, "Subject", "Body");
		TwoFactorAuthConfigurationSingleton singleton = TwoFactorAuthConfigurationSingleton.getInstance();

		withThreadLocalPersistence(persistence, () -> singleton.add(config));

		assertSame(config, singleton.getConfig("test-customer"));
		assertTrue(singleton.isPushTfa("test-customer"));
	}

	@Test
	public void testAddIgnoresNullConfiguration() throws Exception {
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("null-config-customer");
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);

		TwoFactorAuthConfigurationSingleton singleton = TwoFactorAuthConfigurationSingleton.getInstance();

		withThreadLocalPersistence(persistence, () -> singleton.add(null));

		assertNull(singleton.getConfig("null-config-customer"));
	}

	@Test
	public void testShutdownClearsConfiguration() {
		// After shutdown() the in-memory map is cleared; a subsequent getConfig must return null
		TwoFactorAuthConfigurationSingleton singleton = TwoFactorAuthConfigurationSingleton.getInstance();
		singleton.shutdown();
		assertNull(singleton.getConfig("anyCustomer"));
	}

	private static void withThreadLocalPersistence(AbstractPersistence persistence, ThrowingRunnable runnable) throws Exception {
		ThreadLocal<AbstractPersistence> threadLocal = getThreadLocalPersistence();
		AbstractPersistence previous = threadLocal.get();
		try {
			threadLocal.set(persistence);
			runnable.run();
		}
		finally {
			if (previous == null) {
				threadLocal.remove();
			}
			else {
				threadLocal.set(previous);
			}
		}
	}

	private static ThreadLocal<AbstractPersistence> getThreadLocalPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		return threadLocal;
	}

	private interface ThrowingRunnable {
		void run() throws Exception;
	}
}
