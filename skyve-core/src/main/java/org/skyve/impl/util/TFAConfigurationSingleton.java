package org.skyve.impl.util;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.concurrent.ConcurrentHashMap;

import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;

public class TFAConfigurationSingleton {

	private ConcurrentHashMap<String, TwoFactorCustomerConfiguration> hashMap = new ConcurrentHashMap<>();

	private static TFAConfigurationSingleton instance;

	private TFAConfigurationSingleton() {
	}

	public static TFAConfigurationSingleton getInstance() {
		if (instance == null) {
			instance = new TFAConfigurationSingleton();
		}

		return instance;
	}
	
	public static boolean isPushTfa(TwoFactorCustomerConfiguration tfaConfig) {
		if (tfaConfig == null) {
			return false;
		}
		return tfaConfig.isTfaEmail();
	}
	
	public boolean isPushTfa(String customer) {
		if (customer == null) {
			return false;
		}
		return isPushTfa(getConfig(customer));
	}
	
	
	public TwoFactorCustomerConfiguration getConfig(String customer) {
		return hashMap.get(customer);
	}
	
	public void clearConfig(String customerName) {
		hashMap.remove(customerName);
	}
	
	public void add(TwoFactorCustomerConfiguration config) {
		
		String customerName = CORE.getCustomer().getName();

		UtilImpl.LOGGER.info("ELTRACEDEV adding " + customerName + " config " + config.getTfaType());
		
		if (customerName != null && config != null) {
			
			hashMap.put(customerName, config);
		}
		
	}
	
	public void startup(Connection connection) {
		
		this.hashMap.clear();
		
		try (Connection c = connection) {
			
			String query = String.format("select %s,%s,%s,%s,%s from ADM_Configuration where %s is not null and %s is not null",
					"twoFactorType",
					"twofactorPushCodeTimeOutSeconds",
					"twoFactorEmailSubject",
					"twoFactorEmailBody",
					"bizCustomer",
					"twoFactorType",	
					"twofactorPushCodeTimeOutSeconds");
			
			TwoFactorCustomerConfiguration config = null;
			String customer = null;
			try (PreparedStatement s = c.prepareStatement(query)) {
				try (ResultSet rs = s.executeQuery()) {
					while (rs.next()) {
						
						config = new TwoFactorCustomerConfiguration(
								rs.getString(1), 
								rs.getInt(2), 
								rs.getString(3),
								rs.getString(4)
								);
						customer = rs.getString(5);
					}
				}
				
				if (customer != null && config != null) {
					UtilImpl.LOGGER.info("ELTRACEDEV loadinging " + customer + " config " + config.getTfaType());
					this.hashMap.put(customer, config);
				}
				
			}
		} catch (SQLException e) {
			throw new DomainException("Failure reading customer configuration from database.");
		}
		
	}
}
