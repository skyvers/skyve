package org.skyve.impl.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

@SuppressWarnings("static-method")
public class TwoFactorAuthConfigurationSingletonTest {

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
	public void testShutdownClearsConfiguration() {
		// After shutdown() the in-memory map is cleared; a subsequent getConfig must return null
		TwoFactorAuthConfigurationSingleton singleton = TwoFactorAuthConfigurationSingleton.getInstance();
		singleton.shutdown();
		assertNull(singleton.getConfig("anyCustomer"));
	}
}
