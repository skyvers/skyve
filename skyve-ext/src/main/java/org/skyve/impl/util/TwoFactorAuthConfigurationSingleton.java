package org.skyve.impl.util;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLSyntaxErrorException;
import java.util.Locale;
import java.util.concurrent.ConcurrentHashMap;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.DomainException;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

/**
 * Caches and serves customer-level two-factor-authentication configuration.
 *
 * <p>The cache is populated from {@code ADM_Configuration} at startup and keyed by
 * customer name. This singleton is also registered as a {@link SystemObserver}
 * lifecycle participant.
 *
 * <p>Threading: internal storage uses a concurrent map and is safe for concurrent
 * reads and updates.
 */
public class TwoFactorAuthConfigurationSingleton implements SystemObserver {
	private static final String ADM_CONFIGURATION_TABLE_NAME = "adm_configuration";
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(TwoFactorAuthConfigurationSingleton.class);
	private static TwoFactorAuthConfigurationSingleton instance = new TwoFactorAuthConfigurationSingleton();

	private final ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration> configuration = new ConcurrentHashMap<>();

	private TwoFactorAuthConfigurationSingleton() {
		// disallow instantiation
	}

	/**
	 * Returns the singleton instance.
	 *
	 * @return Global two-factor-auth configuration singleton
	 */
	public static TwoFactorAuthConfigurationSingleton getInstance() {
		return instance;
	}
	
	/**
	 * Determines whether push/email-style TFA is enabled for a configuration object.
	 *
	 * @param tfaConfig Customer configuration to test
	 * @return {@code true} when the configuration resolves to email TFA
	 */
	public static boolean isPushTfa(TwoFactorAuthCustomerConfiguration tfaConfig) {
		if (tfaConfig == null) {
			return false;
		}
		return tfaConfig.isTfaEmail();
	}
	
	/**
	 * Determines whether push/email-style TFA is enabled for a customer.
	 *
	 * @param customerName Customer name key
	 * @return {@code true} when the customer has email TFA configured
	 */
	public boolean isPushTfa(String customerName) {
		if (customerName == null) {
			return false;
		}
		return isPushTfa(getConfig(customerName));
	}
	
	
	/**
	 * Returns cached two-factor configuration for a customer.
	 *
	 * @param customerName Customer name key
	 * @return Cached configuration, or {@code null} when absent
	 */
	public TwoFactorAuthCustomerConfiguration getConfig(String customerName) {
		return configuration.get(customerName);
	}
	
	/**
	 * Removes cached two-factor configuration for a customer.
	 *
	 * @param customerName Customer name key
	 */
	public void clearConfig(String customerName) {
		configuration.remove(customerName);
	}
	
	/**
	 * Adds/overwrites configuration for the current thread customer.
	 *
	 * @param config Customer configuration to cache
	 */
	public void add(TwoFactorAuthCustomerConfiguration config) {
		String customerName = CORE.getCustomer().getName();
		if (customerName != null && config != null) {
			configuration.put(customerName, config);
		}
	}
	
	/**
	 * Loads all configured customer TFA settings from the admin configuration table.
	 *
	 * <p>Only rows with complete TFA settings are loaded.
	 */
	@Override
	public void startup() {
		try (Connection c = getDataStoreConnection()) {
			String query = "select twoFactorType, twofactorPushCodeTimeOutSeconds, twoFactorEmailSubject, twoFactorEmailBody, bizCustomer " +
								"from ADM_Configuration " +
								"where twoFactorType is not null and twofactorPushCodeTimeOutSeconds is not null";
			try (PreparedStatement s = c.prepareStatement(query)) {
				try (ResultSet rs = s.executeQuery()) {
					while (rs.next()) {
						String type = rs.getString(1);
						int timeout = rs.getInt(2);
						boolean nullTimeout = rs.wasNull();
						String subject = rs.getString(3);
						String body = rs.getString(4);
						if ((type != null) && (! nullTimeout) && (subject != null) && (body != null)) {
							TwoFactorAuthCustomerConfiguration config = new TwoFactorAuthCustomerConfiguration(type, timeout, subject, body);
							configuration.put(rs.getString(5), config);
						}
					}
				}
			}
		}
		catch (SQLException e) {
			if (shouldIgnoreStartupFailure(e)) {
				LOGGER.warn("Skipping TFA database configuration read during non-production bootstrap.", e);
				return;
			}
			throw new DomainException("Failure reading customer configuration from database.", e);
		}
	}

	/**
	 * Obtains a JDBC connection to the configured Skyve datastore.
	 *
	 * @return An open datastore connection
	 * @throws SQLException If the connection cannot be created
	 */
	Connection getDataStoreConnection() throws SQLException {
		return EXT.getDataStoreConnection();
	}

	/**
	 * Determines whether startup database-read failures should be tolerated.
	 *
	 * <p>This is limited to non-production bootstrap-style environments where
	 * customer-level two-factor authentication has been pre-configured in JSON and
	 * the configuration table may not yet exist.
	 *
	 * @param exception The SQL failure raised while reading TFA settings
	 * @return {@code true} when startup should continue despite the failure
	 */
	@SuppressWarnings("static-method")
	boolean shouldIgnoreStartupFailure(SQLException exception) {
		return (UtilImpl.ENVIRONMENT_IDENTIFIER != null) &&
				(UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS != null) &&
				(! UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS.isEmpty()) &&
				isMissingConfigurationTableFailure(exception);
	}

	private static boolean isMissingConfigurationTableFailure(SQLException exception) {
		for (SQLException current = exception; current != null; current = current.getNextException()) {
			String state = current.getSQLState();
			String message = current.getMessage();
			boolean syntaxOrObjectState = (state != null) && state.startsWith("42");
			boolean missingConfigurationTable = (message != null) &&
					message.toLowerCase(Locale.ROOT).contains(ADM_CONFIGURATION_TABLE_NAME);
			if (missingConfigurationTable && (syntaxOrObjectState || (current instanceof SQLSyntaxErrorException))) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Clears all cached TFA settings.
	 */
	@Override
	public void shutdown() {
		configuration.clear();
	}
}
