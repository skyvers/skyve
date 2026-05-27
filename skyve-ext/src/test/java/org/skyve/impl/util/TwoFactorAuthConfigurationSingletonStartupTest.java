package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;

import java.sql.SQLException;
import java.sql.SQLSyntaxErrorException;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;

class TwoFactorAuthConfigurationSingletonStartupTest {
	private String previousEnvironmentIdentifier = UtilImpl.ENVIRONMENT_IDENTIFIER;
	private Set<String> previousTwoFactorAuthCustomers = UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS;

	@AfterEach
	void restoreUtilSettings() {
		UtilImpl.ENVIRONMENT_IDENTIFIER = previousEnvironmentIdentifier;
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = previousTwoFactorAuthCustomers;
		TwoFactorAuthConfigurationSingleton.getInstance().shutdown();
	}

	@Test
	void startupSkipsDatabaseFailureForConfiguredCustomersInNonProductionBootstrap() throws SQLException {
		UtilImpl.ENVIRONMENT_IDENTIFIER = "development";
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = new HashSet<>(Set.of("acme"));

		TwoFactorAuthConfigurationSingleton singleton = spy(TwoFactorAuthConfigurationSingleton.getInstance());
		doThrow(new SQLSyntaxErrorException("ADM_Configuration does not exist"))
				.when(singleton)
				.getDataStoreConnection();

		assertDoesNotThrow(singleton::startup);
	}

	@Test
	void startupStillFailsForDatabaseFailureOutsideBootstrapScenario() throws SQLException {
		UtilImpl.ENVIRONMENT_IDENTIFIER = null;
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = new HashSet<>(Set.of("acme"));

		TwoFactorAuthConfigurationSingleton singleton = spy(TwoFactorAuthConfigurationSingleton.getInstance());
		doThrow(new SQLSyntaxErrorException("ADM_Configuration does not exist"))
				.when(singleton)
				.getDataStoreConnection();

		assertThrows(DomainException.class, singleton::startup);
	}

	@Test
	void startupStillFailsForUnrelatedSqlFailureInBootstrapScenario() throws SQLException {
		UtilImpl.ENVIRONMENT_IDENTIFIER = "development";
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = new HashSet<>(Set.of("acme"));

		TwoFactorAuthConfigurationSingleton singleton = spy(TwoFactorAuthConfigurationSingleton.getInstance());
		doThrow(new SQLException("Connection refused", "08001"))
				.when(singleton)
				.getDataStoreConnection();

		assertThrows(DomainException.class, singleton::startup);
	}
}
