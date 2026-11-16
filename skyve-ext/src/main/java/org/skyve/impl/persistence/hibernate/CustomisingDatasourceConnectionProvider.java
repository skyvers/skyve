package org.skyve.impl.persistence.hibernate;

import java.sql.Connection;
import java.sql.SQLException;

import org.hibernate.engine.jdbc.connections.internal.DatasourceConnectionProviderImpl;

/**
 * Hibernate's JNDI data source connection provider, customising each connection for no user as it is taken.
 * Registered by {@link AbstractHibernatePersistence} only when {@code factories.connectionCustomiserClass} is set.
 * The persistence customises its connection for its user when it begins a transaction.
 */
public class CustomisingDatasourceConnectionProvider extends DatasourceConnectionProviderImpl {
	private static final long serialVersionUID = 1L;

	@Override
	@SuppressWarnings("resource") // we don't own the resulting connection
	public Connection getConnection() throws SQLException {
		Connection result = super.getConnection();
		CustomisingConnectionProviders.customise(this, result);
		return result;
	}
}
