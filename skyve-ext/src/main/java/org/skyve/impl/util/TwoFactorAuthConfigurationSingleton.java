package org.skyve.impl.util;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.concurrent.ConcurrentHashMap;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.DomainException;

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
	private static final String ADM_CONFIGURATION_TABLE_NAME = "ADM_Configuration";
	private static final String TFA_CUSTOMERS_JSON_PATH = "account.tfaCustomers";
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
		try (Connection c = EXT.getDataStoreConnection()) {
			String query = "select twoFactorType, twofactorPushCodeTimeOutSeconds, twoFactorEmailSubject, twoFactorEmailBody, bizCustomer " +
								"from " + ADM_CONFIGURATION_TABLE_NAME + " " +
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
			throw new DomainException(buildStartupFailureMessage(), e);
		}
	}

	private static String buildStartupFailureMessage() {
		boolean tfaCustomersConfigured = (UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS != null) &&
											(! UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS.isEmpty());
		StringBuilder message = new StringBuilder(512);
		message.append("Failure reading two-factor authentication customer configuration from database table ");
		message.append(ADM_CONFIGURATION_TABLE_NAME);
		message.append(". Skyve reads this table during every application startup to preload customer TFA settings, even when ");
		message.append(TFA_CUSTOMERS_JSON_PATH);
		message.append(" is empty. Troubleshooting: confirm schema synchronisation or migrations completed successfully, confirm ");
		message.append(ADM_CONFIGURATION_TABLE_NAME);
		message.append(" exists in the configured datastore/schema, and check whether ");
		message.append(TFA_CUSTOMERS_JSON_PATH);
		message.append(" is configured in JSON. ");
		message.append(TFA_CUSTOMERS_JSON_PATH);
		message.append(" configured: ");
		message.append(tfaCustomersConfigured);
		message.append('.');
		return message.toString();
	}
	
	/**
	 * Clears all cached TFA settings.
	 */
	@Override
	public void shutdown() {
		configuration.clear();
	}
}
