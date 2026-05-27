package org.skyve.impl.web.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.sql.Timestamp;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.TwoFactorAuthConfigurationSingleton;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.DataStore;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;

import javax.sql.DataSource;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@SuppressWarnings({"static-method", "unchecked", "resource"})
class SkyveSpringSecurityTest {
	private final int originalLockoutThreshold = UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD;
	private final int originalLockoutDurationMultiple = UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS;
	private final DataStore originalDataStore = UtilImpl.DATA_STORE;

	@AfterEach
	void cleanup() {
		UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD = originalLockoutThreshold;
		UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS = originalLockoutDurationMultiple;
		UtilImpl.DATA_STORE = originalDataStore;
		TwoFactorAuthConfigurationSingleton.getInstance().clearConfig("acme");
		TwoFactorAuthConfigurationSingleton.getInstance().clearConfig("beta");
	}

	@Test
	void lockoutHelpersRespectThresholdAndDuration() {
		UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD = 3;
		UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS = 10;
		long now = 200_000L;
		long recentFailure = now - 5_000L;
		long oldFailure = now - 40_000L;

		assertFalse(SkyveSpringSecurity.hasActiveLockout(2, recentFailure, now));
		assertTrue(SkyveSpringSecurity.hasActiveLockout(3, recentFailure, now));
		assertFalse(SkyveSpringSecurity.hasActiveLockout(3, oldFailure, now));
		assertFalse(SkyveSpringSecurity.hasActiveLockout(3, (Timestamp) null, now));
		assertEquals(40_000L, SkyveSpringSecurity.lockoutDurationMillis(4));
	}

	@Test
	void useTFAPushCodeAsPasswordUsesCustomerConfigurationAndExpiry() throws Exception {
		putTfaConfig("acme", createTfaConfig("EMAIL"));
		putTfaConfig("beta", createTfaConfig("OFF"));
		Method method = SkyveSpringSecurity.class.getDeclaredMethod("useTFAPushCodeAsPassword", Timestamp.class, String.class);
		method.setAccessible(true);
		Timestamp now = new Timestamp(System.currentTimeMillis());
		Timestamp old = new Timestamp(System.currentTimeMillis() - 120_000L);

		assertTrue(((Boolean) method.invoke(null, now, "acme")).booleanValue());
		assertFalse(((Boolean) method.invoke(null, old, "acme")).booleanValue());
		assertFalse(((Boolean) method.invoke(null, now, "beta")).booleanValue());
		assertFalse(((Boolean) method.invoke(null, now, "missing")).booleanValue());
		assertFalse(((Boolean) method.invoke(null, null, "acme")).booleanValue());
	}

	@Test
	void createsUtilityBeansForSecurityIntegration() {
		SkyveSpringSecurity security = new SkyveSpringSecurity();
		assertNotNull(security.passwordEncoder());
		UserDetailsService service = security.testUserDetailsService("cust", "user", "password");
		UserDetails loaded = service.loadUserByUsername("cust/user");
		assertNotNull(loaded);
		assertEquals("cust/user", loaded.getUsername());
	}

	@Test
	void providesNoOpDataSourceWhenNoJndiConfigured() throws Exception {
		DataStore dataStore = mock(DataStore.class);
		when(dataStore.getJndiDataSourceName()).thenReturn(null);
		UtilImpl.DATA_STORE = dataStore;

		SkyveSpringSecurity security = new SkyveSpringSecurity();
		DataSource dataSource = security.dataSource();

		assertNotNull(dataSource);
		assertFalse(dataSource.isWrapperFor(DataSource.class));
		assertEquals(30, dataSource.getLoginTimeout());
		assertNull(dataSource.getLogWriter());
		assertNull(dataSource.unwrap(DataSource.class));
		assertDoesNotThrow(() -> dataSource.setLoginTimeout(15));
		assertDoesNotThrow(() -> dataSource.setLogWriter(null));
		assertNull(dataSource.getParentLogger());
	}

	private static Object createTfaConfig(String type) throws Exception {
		Class<?> configClass = Class.forName("org.skyve.impl.util.TwoFactorAuthCustomerConfiguration");
		return configClass.getConstructor(String.class, int.class, String.class, String.class)
				.newInstance(type, Integer.valueOf(60), "subject", "body");
	}

	private static void putTfaConfig(String customer, Object config) throws Exception {
		Field configurationField = TwoFactorAuthConfigurationSingleton.class.getDeclaredField("configuration");
		configurationField.setAccessible(true);
		Map<String, Object> configuration = (Map<String, Object>) configurationField.get(TwoFactorAuthConfigurationSingleton.getInstance());
		configuration.put(customer, config);
	}
}
