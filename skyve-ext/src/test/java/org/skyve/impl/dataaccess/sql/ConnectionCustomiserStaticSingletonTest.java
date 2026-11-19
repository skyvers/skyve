package org.skyve.impl.dataaccess.sql;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.dataaccess.sql.ConnectionCustomiser;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DataStore;

/**
 * Tests the optional connection customiser and its use by {@link EXT#getDataStoreConnection(DataStore)}.
 */
@SuppressWarnings({"static-method", "resource"}) // connections are mocks, or are owned by the code under test
class ConnectionCustomiserStaticSingletonTest {
	private static final DataStore H2 = new DataStore("org.h2.Driver", "jdbc:h2:mem:connectionCustomiser", "sa", "",
														"org.hibernate.dialect.H2Dialect");

	/** A customiser that records each call. */
	private static final class Recording implements ConnectionCustomiser {
		final List<Connection> connections = new ArrayList<>();
		final List<User> users = new ArrayList<>();
		final List<DataStore> dataStores = new ArrayList<>();

		@Override
		public void customise(Connection connection, User user, DataStore dataStore) {
			connections.add(connection);
			users.add(user);
			dataStores.add(dataStore);
		}
	}

	/** A customiser that always fails, recording the connection it was given. */
	private static final class Failing implements ConnectionCustomiser {
		Connection connection;

		@Override
		public void customise(Connection c, User user, DataStore dataStore) throws SQLException {
			connection = c;
			throw new SQLException("refused");
		}
	}

	@AfterEach
	void removeCustomiser() {
		ConnectionCustomiserStaticSingleton.set(null);
	}

	@Test
	void customiseDoesNothingWhenNoneIsInstalled() {
		Connection connection = mock(Connection.class);

		ConnectionCustomiserStaticSingleton.customise(connection, null, H2);

		assertNull(ConnectionCustomiserStaticSingleton.get());
		verifyNoInteractions(connection);
	}

	@Test
	void customisePassesTheConnectionUserAndDataStore() {
		Recording recording = new Recording();
		ConnectionCustomiserStaticSingleton.set(recording);
		Connection connection = mock(Connection.class);
		User user = mock(User.class);

		ConnectionCustomiserStaticSingleton.customise(connection, user, H2);

		assertSame(connection, recording.connections.get(0));
		assertSame(user, recording.users.get(0));
		assertSame(H2, recording.dataStores.get(0));
	}

	@Test
	void customiseWrapsAFailureAndLeavesTheConnectionOpen() throws Exception {
		ConnectionCustomiserStaticSingleton.set(new Failing());
		Connection connection = mock(Connection.class);

		DomainException e = assertThrows(DomainException.class,
											() -> ConnectionCustomiserStaticSingleton.customise(connection, null, H2));

		assertTrue(e.getCause() instanceof SQLException);
		verify(connection, never()).close();
	}

	@Test
	void dataStoreConnectionIsCustomisedForNoUserWithoutAPersistence() throws Exception {
		Recording recording = new Recording();
		ConnectionCustomiserStaticSingleton.set(recording);
		assertFalse(AbstractPersistence.isPresent());

		try (Connection connection = EXT.getDataStoreConnection(H2)) {
			assertSame(H2, recording.dataStores.get(0));
			assertNull(recording.users.get(0));
			assertFalse(recording.connections.get(0).getAutoCommit());
		}
		assertEquals(1, recording.connections.size());
	}

	@Test
	void dataStoreConnectionIsClosedAndNotHandedOutWhenCustomisationFails() throws Exception {
		Failing failing = new Failing();
		ConnectionCustomiserStaticSingleton.set(failing);

		assertThrows(DomainException.class, () -> EXT.getDataStoreConnection(H2));

		assertTrue(failing.connection.isClosed());
	}

	@Test
	void dataStoreConnectionIsNotCustomisedWhenNoneIsInstalled() throws Exception {
		try (Connection connection = EXT.getDataStoreConnection(H2)) {
			assertFalse(connection.isClosed());
		}
	}
}
