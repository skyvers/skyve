package org.skyve.impl.persistence.hibernate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.dataaccess.sql.ConnectionCustomiser;
import org.skyve.dataaccess.sql.SQLDataAccess;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.dataaccess.sql.ConnectionCustomiserStaticSingleton;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DataStore;

import util.AbstractH2Test;

/**
 * Tests that a Hibernate persistence customises its transaction's connection for its user when the transaction
 * begins and when its user is set during the transaction, and that {@link EXT#getDataStoreConnection(DataStore)},
 * and so {@link SQLDataAccess}, customises each connection it hands out for the user of the thread's persistence.
 */
class ConnectionCustomisationH2Test extends AbstractH2Test {
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

	@AfterEach
	@SuppressWarnings("static-method")
	void removeCustomiser() {
		ConnectionCustomiserStaticSingleton.set(null);
	}

	/** A customiser that records the connection it was given, then fails. */
	private static final class Failing implements ConnectionCustomiser {
		@SuppressWarnings("resource") // only recorded; the connection is owned by the code under test
		Connection connection;

		@Override
		public void customise(Connection given, User user, DataStore dataStore) throws SQLException {
			connection = given;
			throw new SQLException("refused");
		}
	}

	private static User anotherUser() {
		SuperUser result = new SuperUser();
		result.setCustomerName(CUSTOMER);
		result.setName("Another");
		result.setId("Another");
		return result;
	}

	@Test
	@SuppressWarnings({ "static-method", "resource" })
	void beginCustomisesTheTransactionConnectionForThePersistenceUser() {
		AbstractHibernatePersistence persistence = (AbstractHibernatePersistence) AbstractPersistence.get();
		persistence.rollback();
		Recording recording = new Recording();
		ConnectionCustomiserStaticSingleton.set(recording);

		persistence.begin();

		assertEquals(1, recording.connections.size());
		assertSame(persistence.getConnection(), recording.connections.get(0));
		assertSame(persistence.getUser(), recording.users.get(0));
		assertSame(UtilImpl.DATA_STORE, recording.dataStores.get(0));
	}

	@Test
	@SuppressWarnings("static-method")
	void beginWithinATransactionDoesNotCustomiseAgain() {
		AbstractPersistence persistence = AbstractPersistence.get();
		Recording recording = new Recording();
		ConnectionCustomiserStaticSingleton.set(recording);

		persistence.begin();

		assertTrue(recording.connections.isEmpty());
	}

	@Test
	@SuppressWarnings({ "static-method", "resource" })
	void setUserDuringATransactionCustomisesForTheNewUser() {
		AbstractHibernatePersistence persistence = (AbstractHibernatePersistence) AbstractPersistence.get();
		User original = persistence.getUser();
		Recording recording = new Recording();
		ConnectionCustomiserStaticSingleton.set(recording);
		User another = anotherUser();
		try {
			persistence.setUser(another);

			assertEquals(1, recording.users.size());
			assertSame(another, recording.users.get(0));
			assertSame(persistence.getConnection(), recording.connections.get(0));
		}
		finally {
			persistence.setUser(original);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void setUserOutsideATransactionDoesNotCustomise() {
		AbstractPersistence persistence = AbstractPersistence.get();
		User original = persistence.getUser();
		persistence.rollback();
		Recording recording = new Recording();
		ConnectionCustomiserStaticSingleton.set(recording);
		try {
			persistence.setUser(anotherUser());

			assertTrue(recording.users.isEmpty());
		}
		finally {
			persistence.setUser(original);
			persistence.begin();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void dataStoreConnectionsAreCustomisedForTheThreadPersistenceUser() throws SQLException {
		Recording recording = new Recording();
		ConnectionCustomiserStaticSingleton.set(recording);

		try (Connection connection = EXT.getDataStoreConnection()) {
			assertEquals(1, recording.connections.size());
			assertSame(AbstractPersistence.get().getUser(), recording.users.get(0));
			assertSame(UtilImpl.DATA_STORE, recording.dataStores.get(0));
			assertTrue(connection.isValid(1));
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void dataStoreConnectionsOnAThreadWithNoPersistenceAreCustomisedForNoUser() throws Exception {
		Recording recording = new Recording();
		ConnectionCustomiserStaticSingleton.set(recording);

		boolean[] persistencePresent = new boolean[1];
		Exception[] failure = new Exception[1];
		Thread thread = new Thread(() -> {
			persistencePresent[0] = AbstractPersistence.isPresent();
			try (Connection connection = EXT.getDataStoreConnection()) {
				// customised as it is handed out
			}
			catch (Exception e) {
				failure[0] = e;
			}
		});
		thread.start();
		thread.join();

		assertNull(failure[0]);
		assertFalse(persistencePresent[0]);
		assertEquals(1, recording.users.size());
		assertNull(recording.users.get(0));
	}

	@Test
	@SuppressWarnings("static-method")
	void aDataStoreConnectionThatCannotBeCustomisedIsClosedAndNotHandedOut() throws SQLException {
		Failing failing = new Failing();
		ConnectionCustomiserStaticSingleton.set(failing);

		assertThrows(DomainException.class, EXT::getDataStoreConnection);

		assertTrue(failing.connection.isClosed());
	}

	@Test
	@SuppressWarnings("static-method")
	void sqlDataAccessCustomisesItsConnectionOnceForTheThreadPersistenceUser() throws Exception {
		Recording recording = new Recording();
		ConnectionCustomiserStaticSingleton.set(recording);

		try (SQLDataAccess dataAccess = EXT.newSQLDataAccess()) {
			assertTrue(recording.connections.isEmpty());

			dataAccess.newSQL("select 1").scalarResult(Number.class);
			dataAccess.newSQL("select 2").scalarResult(Number.class);

			assertEquals(1, recording.connections.size());
			assertSame(AbstractPersistence.get().getUser(), recording.users.get(0));
		}
	}
}
