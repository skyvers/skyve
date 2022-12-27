package org.skyve.impl.util;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.concurrent.ConcurrentHashMap;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.controller.Observer;

public class TwoFactorAuthConfigurationSingleton implements Observer {
	private static TwoFactorAuthConfigurationSingleton instance;

	private final ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration> configuration = new ConcurrentHashMap<>();

	private TwoFactorAuthConfigurationSingleton() {
		// disallow instantiation
	}

	public static TwoFactorAuthConfigurationSingleton getInstance() {
		if (instance == null) {
			instance = new TwoFactorAuthConfigurationSingleton();
		}

		return instance;
	}
	
	public static boolean isPushTfa(TwoFactorAuthCustomerConfiguration tfaConfig) {
		if (tfaConfig == null) {
			return false;
		}
		return tfaConfig.isTfaEmail();
	}
	
	public boolean isPushTfa(String customerName) {
		if (customerName == null) {
			return false;
		}
		return isPushTfa(getConfig(customerName));
	}
	
	
	public TwoFactorAuthCustomerConfiguration getConfig(String customerName) {
		return configuration.get(customerName);
	}
	
	public void clearConfig(String customerName) {
		configuration.remove(customerName);
	}
	
	public void add(TwoFactorAuthCustomerConfiguration config) {
		String customerName = CORE.getCustomer().getName();
		if (customerName != null && config != null) {
			configuration.put(customerName, config);
		}
	}
	
	@Override
	public void startup() {
		try (Connection c = EXT.getDataStoreConnection()) {
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
			throw new DomainException("Failure reading customer configuration from database.", e);
		}
	}
	
	@Override
	public void shutdown() {
		configuration.clear();
	}
}
