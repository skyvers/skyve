package org.skyve.impl.persistence.hibernate;

import java.sql.Connection;
import java.sql.SQLException;

import org.hibernate.engine.jdbc.connections.internal.DriverManagerConnectionProviderImpl;

/**
 * Hibernate's driver manager connection provider, customising each connection for no user as it is taken.
 * Registered by {@link AbstractHibernatePersistence} only when {@code factories.connectionCustomiserClass} is set
 * and the data store names no JNDI data source.
 * The persistence customises its connection for its user when it begins a transaction.
 */
public class CustomisingDriverManagerConnectionProvider extends DriverManagerConnectionProviderImpl {
	private static final long serialVersionUID = 1L;

	@Override
	@SuppressWarnings("resource") // we don't own the resulting connection
	public Connection getConnection() throws SQLException {
		Connection result = super.getConnection();
		CustomisingConnectionProviders.customise(this, result);
		return result;
	}
}
