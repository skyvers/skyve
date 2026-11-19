package org.skyve.impl.persistence.hibernate;

import java.sql.Connection;
import java.sql.SQLException;

import org.hibernate.engine.jdbc.connections.spi.ConnectionProvider;
import org.skyve.impl.dataaccess.sql.ConnectionCustomiserStaticSingleton;
import org.skyve.impl.util.UtilImpl;

import jakarta.annotation.Nonnull;

/**
 * The customisation shared by the customising Hibernate connection providers.
 */
final class CustomisingConnectionProviders {
	private CustomisingConnectionProviders() {
		// static helpers
	}

	/**
	 * Customises a connection just taken for no user. A connection that cannot be customised is returned to
	 * the provider and the failure is thrown, so Hibernate never receives it.
	 *
	 * @param provider	The provider the connection came from.
	 * @param connection	The connection.
	 * @throws SQLException	When the connection cannot be customised.
	 */
	static void customise(@Nonnull ConnectionProvider provider, @Nonnull Connection connection) throws SQLException {
		try {
			ConnectionCustomiserStaticSingleton.customise(connection, null, UtilImpl.DATA_STORE);
		}
		catch (RuntimeException e) {
			try {
				provider.closeConnection(connection);
			}
			catch (SQLException ce) {
				e.addSuppressed(ce);
			}
			throw new SQLException("Could not customise the database connection", e);
		}
	}
}
