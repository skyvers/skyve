package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class TwoFactorAuthConfigurationSingletonStartupTest {
	private String previousEnvironmentIdentifier = UtilImpl.ENVIRONMENT_IDENTIFIER;
	private Set<String> previousTwoFactorAuthCustomers = UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS;

	@AfterEach
	void restoreUtilSettings() {
		UtilImpl.ENVIRONMENT_IDENTIFIER = previousEnvironmentIdentifier;
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = previousTwoFactorAuthCustomers;
		TwoFactorAuthConfigurationSingleton.getInstance().shutdown();
	}

	private static String invokeStartupFailureMessageBuilder() {
		try {
			Method method = TwoFactorAuthConfigurationSingleton.class.getDeclaredMethod("buildStartupFailureMessage");
			method.setAccessible(true);
			return (String) method.invoke(null);
		}
		catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	@Test
	void startupFailureMessageIdentifiesConfigurationTableAndConfiguredJsonTfaCustomers() {
		UtilImpl.ENVIRONMENT_IDENTIFIER = "development";
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = new HashSet<>(Set.of("acme"));

		String message = invokeStartupFailureMessageBuilder();

		assertTrue(message.contains("ADM_Configuration"));
		assertTrue(message.contains("schema synchronisation"));
		assertTrue(message.contains("every application startup"));
		assertTrue(message.contains("even when account.tfaCustomers is empty"));
		assertTrue(message.contains("account.tfaCustomers configured: true"));
	}

	@Test
	void startupFailureMessageReportsWhenJsonTfaCustomersAreNotConfigured() {
		UtilImpl.ENVIRONMENT_IDENTIFIER = "development";
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = new HashSet<>();

		String message = invokeStartupFailureMessageBuilder();

		assertTrue(message.contains("account.tfaCustomers configured: false"));
	}
}
