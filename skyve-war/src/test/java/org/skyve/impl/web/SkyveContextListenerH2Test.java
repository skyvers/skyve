package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import java.lang.reflect.Method;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;

import util.AbstractH2TestDispose;

class SkyveContextListenerH2Test extends AbstractH2TestDispose {
	@Test
	@SuppressWarnings("static-method")
	void initialiseSchemaAndBootstrapCommitsWhenBootstrapIsNotConfigured() throws Exception {
		String originalEnvironmentIdentifier = UtilImpl.ENVIRONMENT_IDENTIFIER;
		String originalBootstrapCustomer = UtilImpl.BOOTSTRAP_CUSTOMER;
		try {
			UtilImpl.ENVIRONMENT_IDENTIFIER = null;
			UtilImpl.BOOTSTRAP_CUSTOMER = null;

			Method method = SkyveContextListener.class.getDeclaredMethod("initialiseSchemaAndBootstrap");
			method.setAccessible(true);

			assertDoesNotThrow(() -> method.invoke(null));
		}
		finally {
			UtilImpl.ENVIRONMENT_IDENTIFIER = originalEnvironmentIdentifier;
			UtilImpl.BOOTSTRAP_CUSTOMER = originalBootstrapCustomer;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void initialiseSchemaAndBootstrapCreatesConfiguredBootstrapUser() throws Exception {
		String originalEnvironmentIdentifier = UtilImpl.ENVIRONMENT_IDENTIFIER;
		String originalBootstrapCustomer = UtilImpl.BOOTSTRAP_CUSTOMER;
		String originalBootstrapUser = UtilImpl.BOOTSTRAP_USER;
		String originalBootstrapEmail = UtilImpl.BOOTSTRAP_EMAIL;
		String originalBootstrapPassword = UtilImpl.BOOTSTRAP_PASSWORD;
		try {
			UtilImpl.ENVIRONMENT_IDENTIFIER = "h2";
			UtilImpl.BOOTSTRAP_CUSTOMER = "bizhub";
			UtilImpl.BOOTSTRAP_USER = "bootstrap_" + UUID.randomUUID().toString().replace("-", "");
			UtilImpl.BOOTSTRAP_EMAIL = UtilImpl.BOOTSTRAP_USER + "@example.test";
			UtilImpl.BOOTSTRAP_PASSWORD = "TestPassword0!";

			Method method = SkyveContextListener.class.getDeclaredMethod("initialiseSchemaAndBootstrap");
			method.setAccessible(true);

			assertDoesNotThrow(() -> method.invoke(null));
		}
		finally {
			UtilImpl.ENVIRONMENT_IDENTIFIER = originalEnvironmentIdentifier;
			UtilImpl.BOOTSTRAP_CUSTOMER = originalBootstrapCustomer;
			UtilImpl.BOOTSTRAP_USER = originalBootstrapUser;
			UtilImpl.BOOTSTRAP_EMAIL = originalBootstrapEmail;
			UtilImpl.BOOTSTRAP_PASSWORD = originalBootstrapPassword;
		}
	}
}
