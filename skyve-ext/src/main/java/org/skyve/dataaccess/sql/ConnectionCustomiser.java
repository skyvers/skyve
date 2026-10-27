package org.skyve.dataaccess.sql;

import java.sql.Connection;
import java.sql.SQLException;

import org.skyve.metadata.user.User;
import org.skyve.persistence.DataStore;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Prepares each database connection that Skyve hands out for the user it serves.
 * Named by the {@code factories.connectionCustomiserClass} setting; when it is not set, connections are not customised.
 *
 * <p>Skyve calls {@link #customise(Connection, User, DataStore)}:
 * <ul>
 * <li>with no user, each time Hibernate takes a connection from the data store, so that a connection taken
 * outside a transaction never keeps state from its previous use;</li>
 * <li>with the persistence's user, when a persistence begins a transaction, and when its user is set while
 * a transaction is active;</li>
 * <li>with the thread's persistence's user, or no user when there is none, for each connection from
 * {@link org.skyve.EXT#getDataStoreConnection(DataStore)}, which also serves {@link org.skyve.EXT#newSQLDataAccess()}.</li>
 * </ul>
 * A connection is therefore customised on every checkout, and an implementation must set its state every time,
 * clearing it when there is no user, because a pooled connection keeps session state between uses.
 *
 * <p>Implementations are created once at startup and called concurrently from many threads, so they must be thread safe.
 * The connection is not in auto-commit mode; an implementation must not commit, roll back or close it.
 */
public interface ConnectionCustomiser {
	/**
	 * Prepares a connection for a user.
	 *
	 * @param connection	The connection, already taken from the data store and not in auto-commit mode.
	 * @param user	The user the connection will serve, or {@code null} when there is none.
	 * @param dataStore	The data store the connection belongs to.
	 * @throws SQLException	When the connection cannot be prepared. Skyve then does not hand the connection out.
	 */
	void customise(@Nonnull Connection connection, @Nullable User user, @Nonnull DataStore dataStore)
	throws SQLException;
}
