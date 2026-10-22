package org.skyve.impl.persistence.hibernate;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.sql.Connection;
import java.sql.SQLException;

import org.hibernate.engine.jdbc.connections.spi.ConnectionProvider;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.dataaccess.sql.ConnectionCustomiserStaticSingleton;
import org.skyve.metadata.user.User;

/**
 * Tests the customisation that the customising Hibernate connection providers apply to each connection taken.
 */
@SuppressWarnings({"static-method", "resource"}) // connections are mocks
class CustomisingConnectionProvidersTest {
	@AfterEach
	void removeCustomiser() {
		ConnectionCustomiserStaticSingleton.set(null);
	}

	@Test
	void connectionIsCustomisedForNoUser() throws Exception {
		Connection[] customised = new Connection[1];
		User[] users = { mock(User.class) };
		ConnectionCustomiserStaticSingleton.set((c, u, d) -> {
			customised[0] = c;
			users[0] = u;
		});
		ConnectionProvider provider = mock(ConnectionProvider.class);
		Connection connection = mock(Connection.class);

		CustomisingConnectionProviders.customise(provider, connection);

		assertSame(connection, customised[0]);
		assertNull(users[0]);
		verify(provider, never()).closeConnection(connection);
	}

	@Test
	void connectionIsReturnedToTheProviderWhenCustomisationFails() throws Exception {
		ConnectionCustomiserStaticSingleton.set((c, u, d) -> {
			throw new SQLException("refused");
		});
		ConnectionProvider provider = mock(ConnectionProvider.class);
		Connection connection = mock(Connection.class);

		SQLException e = assertThrows(SQLException.class, () -> CustomisingConnectionProviders.customise(provider, connection));

		assertTrue(e.getCause() instanceof DomainException);
		verify(provider).closeConnection(connection);
	}
}
