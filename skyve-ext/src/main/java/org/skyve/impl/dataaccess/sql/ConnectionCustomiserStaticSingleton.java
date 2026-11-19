package org.skyve.impl.dataaccess.sql;

import java.sql.Connection;
import java.sql.SQLException;

import org.skyve.dataaccess.sql.ConnectionCustomiser;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DataStore;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A singleton for the optional {@link ConnectionCustomiser} named by {@code factories.connectionCustomiserClass}.
 * NB This doesn't need to be thread safe as it is set at startup only.
 *
 * <p>Tests can install an implementation with {@link #set(ConnectionCustomiser)}, and must remove it with
 * {@code set(null)} afterwards. Such tests must not run in parallel threads in the same JVM.
 */
public class ConnectionCustomiserStaticSingleton {
	/** The installed implementation; {@code null} when connections are not customised. */
	private static ConnectionCustomiser instance;

	/**
	 * Disallows instantiation.
	 */
	private ConnectionCustomiserStaticSingleton() {
		// nothing to see here
	}

	/**
	 * Returns the installed implementation.
	 *
	 * @return	The implementation, or {@code null} when connections are not customised.
	 */
	public static @Nullable ConnectionCustomiser get() {
		return instance;
	}

	/**
	 * Installs an implementation.
	 *
	 * @param instance	The implementation to install, or {@code null} to stop customising connections.
	 */
	public static void set(@Nullable ConnectionCustomiser instance) {
		ConnectionCustomiserStaticSingleton.instance = instance;
	}

	/**
	 * Customises a connection with the installed implementation, if there is one.
	 * The connection is left open when the implementation fails; a caller that has not yet handed it out closes it.
	 *
	 * @param connection	The connection to customise.
	 * @param user	The user the connection will serve, or {@code null}.
	 * @param dataStore	The data store the connection belongs to.
	 * @throws DomainException	When the implementation throws an {@link SQLException}.
	 */
	public static void customise(@Nonnull Connection connection, @Nullable User user, @Nonnull DataStore dataStore) {
		ConnectionCustomiser customiser = instance;
		if (customiser != null) {
			try {
				customiser.customise(connection, user, dataStore);
			}
			catch (SQLException e) {
				throw new DomainException("Could not customise the database connection", e);
			}
		}
	}
}
