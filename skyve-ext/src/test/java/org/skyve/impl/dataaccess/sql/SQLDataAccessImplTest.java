package org.skyve.impl.dataaccess.sql;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.SQLException;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.model.document.Document;

@SuppressWarnings({ "static-method", "resource", "boxing" })
class SQLDataAccessImplTest {
	@Test
	void closeWithNoConnectionDoesNothing() {
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);

		assertDoesNotThrow(dataAccess::close);
	}

	@Test
	void closeWithOpenConnectionClosesConnection() throws Exception {
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);
		Connection connection = mock(Connection.class);
		when(connection.isClosed()).thenReturn(Boolean.FALSE);
		setConnection(dataAccess, connection);

		dataAccess.close();

		verify(connection).close();
	}

	@Test
	void closeWithClosedConnectionDoesNotCloseAgain() throws Exception {
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);
		Connection connection = mock(Connection.class);
		when(connection.isClosed()).thenReturn(Boolean.TRUE);
		setConnection(dataAccess, connection);

		dataAccess.close();

		verify(connection, never()).close();
	}

	@Test
	void commitWithOpenConnectionCommits() throws Exception {
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);
		Connection connection = mock(Connection.class);
		when(connection.isClosed()).thenReturn(Boolean.FALSE);
		setConnection(dataAccess, connection);

		dataAccess.commit();

		verify(connection).commit();
	}

	@Test
	void commitWithClosedConnectionSkipsCommit() throws Exception {
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);
		Connection connection = mock(Connection.class);
		when(connection.isClosed()).thenReturn(Boolean.TRUE);
		setConnection(dataAccess, connection);

		dataAccess.commit();

		verify(connection, never()).commit();
	}

	@Test
	void commitWrapsSQLExceptionAsDomainException() throws Exception {
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);
		Connection connection = mock(Connection.class);
		when(connection.isClosed()).thenReturn(Boolean.FALSE);
		doThrowOnCommit(connection);
		setConnection(dataAccess, connection);

		DomainException e = assertThrows(DomainException.class, dataAccess::commit);
		assertInstanceOf(SQLException.class, e.getCause());
	}

	@Test
	void rollbackWithOpenConnectionRollsBack() throws Exception {
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);
		Connection connection = mock(Connection.class);
		when(connection.isClosed()).thenReturn(Boolean.FALSE);
		setConnection(dataAccess, connection);

		dataAccess.rollback();

		verify(connection).rollback();
	}

	@Test
	void rollbackWithClosedConnectionSkipsRollback() throws Exception {
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);
		Connection connection = mock(Connection.class);
		when(connection.isClosed()).thenReturn(Boolean.TRUE);
		setConnection(dataAccess, connection);

		dataAccess.rollback();

		verify(connection, never()).rollback();
	}

	@Test
	void rollbackWrapsSQLExceptionAsDomainException() throws Exception {
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);
		Connection connection = mock(Connection.class);
		when(connection.isClosed()).thenReturn(Boolean.FALSE);
		doThrowOnRollback(connection);
		setConnection(dataAccess, connection);

		DomainException e = assertThrows(DomainException.class, dataAccess::rollback);
		assertInstanceOf(SQLException.class, e.getCause());
	}

	@Test
	void newSQLWithDocumentReturnsSQLDataAccessSQL() {
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);
		Document document = mock(Document.class);

		SQLDataAccessSQL sql = (SQLDataAccessSQL) dataAccess.newSQL(document, "select 1");

		assertInstanceOf(SQLDataAccessSQL.class, sql);
	}

	@Test
	void newSQLWithQueryOnlyReturnsSQLDataAccessSQL() {
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);

		SQLDataAccessSQL sql = (SQLDataAccessSQL) dataAccess.newSQL("select 1");

		assertInstanceOf(SQLDataAccessSQL.class, sql);
	}

	private static void doThrowOnCommit(Connection connection) throws SQLException {
		org.mockito.Mockito.doThrow(new SQLException("commit failed")).when(connection).commit();
	}

	private static void doThrowOnRollback(Connection connection) throws SQLException {
		org.mockito.Mockito.doThrow(new SQLException("rollback failed")).when(connection).rollback();
	}

	private static void setConnection(SQLDataAccessImpl dataAccess, Connection connection) throws Exception {
		Field field = SQLDataAccessImpl.class.getDeclaredField("connection");
		field.setAccessible(true);
		field.set(dataAccess, connection);
	}
}
