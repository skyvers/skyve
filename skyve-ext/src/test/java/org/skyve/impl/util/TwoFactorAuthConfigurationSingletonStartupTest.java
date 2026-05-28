package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
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
	void startupSkipsDatabaseReadWhenConfigurationTableIsMissingInNonProductionBootstrap() throws SQLException {
		UtilImpl.ENVIRONMENT_IDENTIFIER = "development";
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = new HashSet<>(Set.of("acme"));

		TwoFactorAuthConfigurationSingleton singleton = spy(TwoFactorAuthConfigurationSingleton.getInstance());
		Connection connection = connectionWithConfigurationTable(false);
		doReturn(connection).when(singleton).getDataStoreConnection();

		assertDoesNotThrow(singleton::startup);
		verify(connection, never()).prepareStatement(TFA_CONFIGURATION_QUERY);
	}

	@Test
	void startupStillFailsForMissingConfigurationTableOutsideBootstrapScenario() throws SQLException {
		UtilImpl.ENVIRONMENT_IDENTIFIER = null;
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = new HashSet<>(Set.of("acme"));

		TwoFactorAuthConfigurationSingleton singleton = spy(TwoFactorAuthConfigurationSingleton.getInstance());
		Connection connection = mock(Connection.class);
		doReturn(connection).when(singleton).getDataStoreConnection();
		doThrow(new SQLSyntaxErrorException("ADM_Configuration does not exist"))
				.when(connection)
				.prepareStatement(TFA_CONFIGURATION_QUERY);

		assertThrows(DomainException.class, singleton::startup);
	}

	@Test
	void startupStillFailsForDatabaseReadFailureWhenConfigurationTableExistsInBootstrapScenario() throws SQLException {
		UtilImpl.ENVIRONMENT_IDENTIFIER = "development";
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = new HashSet<>(Set.of("acme"));

		TwoFactorAuthConfigurationSingleton singleton = spy(TwoFactorAuthConfigurationSingleton.getInstance());
		Connection connection = connectionWithConfigurationTable(true);
		doReturn(connection).when(singleton).getDataStoreConnection();
		doThrow(new SQLException("Connection refused", "08001"))
				.when(connection)
				.prepareStatement(TFA_CONFIGURATION_QUERY);

		assertThrows(DomainException.class, singleton::startup);
	}

	@Test
	void startupStillFailsForMetadataFailureInBootstrapScenario() throws SQLException {
		UtilImpl.ENVIRONMENT_IDENTIFIER = "development";
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = new HashSet<>(Set.of("acme"));

		TwoFactorAuthConfigurationSingleton singleton = spy(TwoFactorAuthConfigurationSingleton.getInstance());
		Connection connection = mock(Connection.class);
		doReturn(connection).when(singleton).getDataStoreConnection();
		when(connection.getMetaData()).thenThrow(new SQLException("Metadata unavailable"));

		assertThrows(DomainException.class, singleton::startup);
	}

	private static Connection connectionWithConfigurationTable(boolean tableExists) throws SQLException {
		Connection connection = mock(Connection.class);
		DatabaseMetaData metadata = mock(DatabaseMetaData.class);
		ResultSet tables = mock(ResultSet.class);
		when(connection.getMetaData()).thenReturn(metadata);
		when(metadata.getTables(isNull(), isNull(), eq("%"), any(String[].class))).thenReturn(tables);
		if (tableExists) {
			when(tables.next()).thenReturn(true);
			when(tables.getString("TABLE_NAME")).thenReturn("ADM_CONFIGURATION");
			PreparedStatement statement = mock(PreparedStatement.class);
			ResultSet results = mock(ResultSet.class);
			when(connection.prepareStatement(TFA_CONFIGURATION_QUERY)).thenReturn(statement);
			when(statement.executeQuery()).thenReturn(results);
		}
		return connection;
	}
}
