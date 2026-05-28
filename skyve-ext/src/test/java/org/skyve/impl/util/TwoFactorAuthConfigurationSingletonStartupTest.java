package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.SQLSyntaxErrorException;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;

@SuppressWarnings("static-method")
class TwoFactorAuthConfigurationSingletonStartupTest {
	private static final String TFA_CONFIGURATION_QUERY =
			"select twoFactorType, twofactorPushCodeTimeOutSeconds, twoFactorEmailSubject, twoFactorEmailBody, bizCustomer " +
					"from ADM_Configuration " +
					"where twoFactorType is not null and twofactorPushCodeTimeOutSeconds is not null";

	private String previousEnvironmentIdentifier = UtilImpl.ENVIRONMENT_IDENTIFIER;
	private Set<String> previousTwoFactorAuthCustomers = UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS;

	@AfterEach
	void restoreUtilSettings() {
		UtilImpl.ENVIRONMENT_IDENTIFIER = previousEnvironmentIdentifier;
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = previousTwoFactorAuthCustomers;
		TwoFactorAuthConfigurationSingleton.getInstance().shutdown();
	}

	@Test
	void startupFailureMessageIdentifiesConfigurationTableAndConfiguredJsonTfaCustomers() throws SQLException {
		UtilImpl.ENVIRONMENT_IDENTIFIER = "development";
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = new HashSet<>(Set.of("acme"));

		TwoFactorAuthConfigurationSingleton singleton = spy(TwoFactorAuthConfigurationSingleton.getInstance());
		Connection connection = mock(Connection.class);
		doReturn(connection).when(singleton).getDataStoreConnection();
		doThrow(new SQLSyntaxErrorException("ADM_Configuration does not exist"))
				.when(connection)
				.prepareStatement(TFA_CONFIGURATION_QUERY);

		DomainException exception = assertThrows(DomainException.class, singleton::startup);
		String message = exception.getMessage();

		assertTrue(message.contains("ADM_Configuration"));
		assertTrue(message.contains("schema synchronisation"));
		assertTrue(message.contains("account.tfaCustomers configured: true"));
		assertTrue(message.contains("brand-new database"));
	}

	@Test
	void startupFailureMessageReportsWhenJsonTfaCustomersAreNotConfigured() throws SQLException {
		UtilImpl.ENVIRONMENT_IDENTIFIER = "development";
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = new HashSet<>();

		TwoFactorAuthConfigurationSingleton singleton = spy(TwoFactorAuthConfigurationSingleton.getInstance());
		Connection connection = mock(Connection.class);
		doReturn(connection).when(singleton).getDataStoreConnection();
		doThrow(new SQLSyntaxErrorException("ADM_Configuration does not exist"))
				.when(connection)
				.prepareStatement(TFA_CONFIGURATION_QUERY);

		DomainException exception = assertThrows(DomainException.class, singleton::startup);

		assertTrue(exception.getMessage().contains("account.tfaCustomers configured: false"));
	}
}
