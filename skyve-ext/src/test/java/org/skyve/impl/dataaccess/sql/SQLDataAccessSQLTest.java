package org.skyve.impl.dataaccess.sql;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.persistence.AutoClosingIterable;

@SuppressWarnings({ "static-method", "resource", "boxing" })
class SQLDataAccessSQLTest {
	@Test
	void beanResultsWithoutDocumentThrowsDomainException() {
		SQLDataAccessImpl dataAccess = mock(SQLDataAccessImpl.class);
		SQLDataAccessSQL sql = new SQLDataAccessSQL("select 1", dataAccess);

		DomainException e = assertThrows(DomainException.class, sql::beanResults);
		assertEquals("The document must be set to create beans from SQL", e.getMessage());
	}

	@Test
	void beanIterableWithoutDocumentThrowsDomainException() {
		SQLDataAccessImpl dataAccess = mock(SQLDataAccessImpl.class);
		SQLDataAccessSQL sql = new SQLDataAccessSQL("select 1", dataAccess);

		DomainException e = assertThrows(DomainException.class, sql::beanIterable);
		assertEquals("The document must be set to create beans from SQL", e.getMessage());
	}

	@Test
	void scalarResultsReturnsConvertedValues() throws Exception {
		ResultSetMetaData metaData = mock(ResultSetMetaData.class);
		Fixture fixture = fixture(metaData);
		when(fixture.resultSet.next()).thenReturn(Boolean.TRUE, Boolean.FALSE);
		when(fixture.resultSet.getObject(1)).thenReturn(Integer.valueOf(5));
		SQLDataAccessSQL sql = new SQLDataAccessSQL("select 1", fixture.dataAccess);
		sql.noTimeout();

		List<Integer> results = sql.scalarResults(Integer.class);

		assertEquals(1, results.size());
		assertEquals(Integer.valueOf(5), results.get(0));
	}

	@Test
	void scalarIterableReturnsIterable() throws Exception {
		Fixture fixture = fixture(null);
		SQLDataAccessSQL sql = new SQLDataAccessSQL("select 1", fixture.dataAccess);
		sql.noTimeout();

		AutoClosingIterable<Integer> iterable = sql.scalarIterable(Integer.class);

		assertNotNull(iterable);
		iterable.close();
	}

	@Test
	void tupleResultsReturnsRowValues() throws Exception {
		ResultSetMetaData metaData = mock(ResultSetMetaData.class);
		Fixture fixture = fixture(metaData);
		when(metaData.getColumnCount()).thenReturn(Integer.valueOf(2));
		when(fixture.resultSet.next()).thenReturn(Boolean.TRUE, Boolean.FALSE);
		when(fixture.resultSet.getObject(1)).thenReturn("a");
		when(fixture.resultSet.getObject(2)).thenReturn(Integer.valueOf(3));
		SQLDataAccessSQL sql = new SQLDataAccessSQL("select 1, 2", fixture.dataAccess);
		sql.noTimeout();

		List<Object[]> results = sql.tupleResults();

		assertEquals(1, results.size());
		assertEquals("a", results.get(0)[0]);
		assertEquals(Integer.valueOf(3), results.get(0)[1]);
	}

	@Test
	void tupleIterableReturnsIterable() throws Exception {
		Fixture fixture = fixture(null);
		SQLDataAccessSQL sql = new SQLDataAccessSQL("select 1", fixture.dataAccess);
		sql.noTimeout();

		AutoClosingIterable<Object[]> iterable = sql.tupleIterable();

		assertNotNull(iterable);
		iterable.close();
	}

	@Test
	void executeReturnsUpdateCount() throws Exception {
		Fixture fixture = fixture(null);
		when(fixture.preparedStatement.executeUpdate()).thenReturn(Integer.valueOf(2));
		SQLDataAccessSQL sql = new SQLDataAccessSQL("update t set c = 1", fixture.dataAccess);
		sql.noTimeout();

		int updated = sql.execute();

		assertEquals(2, updated);
	}

	@Test
	void executeWrapsSQLExceptionAsDomainException() throws Exception {
		Fixture fixture = fixture(null);
		when(fixture.preparedStatement.executeUpdate()).thenThrow(new SQLException("fail"));
		SQLDataAccessSQL sql = new SQLDataAccessSQL("update t set c = 1", fixture.dataAccess);
		sql.noTimeout();

		DomainException e = assertThrows(DomainException.class, sql::execute);
		assertInstanceOf(SQLException.class, e.getCause());
	}

	@Test
	void dynaResultsWrapsSQLExceptionAsDomainException() throws Exception {
		Connection connection = mock(Connection.class);
		when(connection.prepareStatement(org.mockito.ArgumentMatchers.any(String.class))).thenThrow(new SQLException("dyna fail"));
		SQLDataAccessSQL sql = new SQLDataAccessSQL("select * from t", new TestSQLDataAccessImpl(connection));
		sql.noTimeout();

		DomainException e = assertThrows(DomainException.class, sql::dynaResults);

		assertInstanceOf(SQLException.class, e.getCause());
	}

	@Test
	void dynaResultsRethrowsSkyveException() {
		DomainException expected = new DomainException("connection failed");
		SQLDataAccessSQL sql = new SQLDataAccessSQL("select * from t", new FailingSQLDataAccessImpl(expected));
		sql.noTimeout();

		DomainException e = assertThrows(DomainException.class, sql::dynaResults);

		assertSame(expected, e);
	}

	@Test
	void dynaResultsReturnsDetachedRows() throws Exception {
		ResultSetMetaData metaData = mock(ResultSetMetaData.class);
		Fixture fixture = fixture(metaData);
		when(metaData.getColumnCount()).thenReturn(Integer.valueOf(2));
		when(metaData.getColumnLabel(1)).thenReturn("name");
		when(metaData.getColumnName(1)).thenReturn("name");
		when(metaData.getColumnLabel(2)).thenReturn("score");
		when(metaData.getColumnName(2)).thenReturn("score");
		when(fixture.resultSet.next()).thenReturn(Boolean.TRUE, Boolean.FALSE);
		when(fixture.resultSet.getObject(1)).thenReturn("alpha");
		when(fixture.resultSet.getObject(2)).thenReturn(Integer.valueOf(7));
		when(fixture.resultSet.getObject("name")).thenReturn("alpha");
		when(fixture.resultSet.getObject("score")).thenReturn(Integer.valueOf(7));
		SQLDataAccessSQL sql = new SQLDataAccessSQL("select name, score from t", fixture.dataAccess);
		sql.noTimeout();

		List<org.apache.commons.beanutils.DynaBean> results = sql.dynaResults();

		assertEquals(1, results.size());
		assertEquals("alpha", results.get(0).get("name"));
		assertEquals(Integer.valueOf(7), results.get(0).get("score"));
	}

	@Test
	void dynaIterableWrapsSQLExceptionAsDomainException() throws Exception {
		Connection connection = mock(Connection.class);
		when(connection.prepareStatement(org.mockito.ArgumentMatchers.any(String.class))).thenThrow(new SQLException("iterable fail"));
		SQLDataAccessSQL sql = new SQLDataAccessSQL("select * from t", new TestSQLDataAccessImpl(connection));
		sql.noTimeout();

		DomainException e = assertThrows(DomainException.class, sql::dynaIterable);

		assertInstanceOf(SQLException.class, e.getCause());
	}

	@Test
	void dynaIterableRethrowsSkyveException() {
		DomainException expected = new DomainException("connection failed");
		SQLDataAccessSQL sql = new SQLDataAccessSQL("select * from t", new FailingSQLDataAccessImpl(expected));
		sql.noTimeout();

		DomainException e = assertThrows(DomainException.class, sql::dynaIterable);

		assertSame(expected, e);
	}

	@Test
	void executeRethrowsSkyveException() {
		DomainException expected = new DomainException("connection failed");
		SQLDataAccessSQL sql = new SQLDataAccessSQL("update t set c = 1", new FailingSQLDataAccessImpl(expected));
		sql.noTimeout();

		DomainException e = assertThrows(DomainException.class, sql::execute);

		assertSame(expected, e);
	}

	private static Fixture fixture(ResultSetMetaData metaData) throws SQLException {
		Connection connection = mock(Connection.class);
		PreparedStatement preparedStatement = mock(PreparedStatement.class);
		ResultSet resultSet = mock(ResultSet.class);
		when(connection.prepareStatement(org.mockito.ArgumentMatchers.any(String.class))).thenReturn(preparedStatement);
		when(preparedStatement.executeQuery()).thenReturn(resultSet);
		when(resultSet.isClosed()).thenReturn(Boolean.FALSE);
		if (metaData != null) {
			when(resultSet.getMetaData()).thenReturn(metaData);
		}

		TestSQLDataAccessImpl dataAccess = new TestSQLDataAccessImpl(connection);
		return new Fixture(dataAccess, preparedStatement, resultSet);
	}

	private static final class Fixture {
		private final TestSQLDataAccessImpl dataAccess;
		private final PreparedStatement preparedStatement;
		private final ResultSet resultSet;

		private Fixture(TestSQLDataAccessImpl dataAccess, PreparedStatement preparedStatement, ResultSet resultSet) {
			this.dataAccess = dataAccess;
			this.preparedStatement = preparedStatement;
			this.resultSet = resultSet;
		}
	}

	private static final class TestSQLDataAccessImpl extends SQLDataAccessImpl {
		private final Connection connection;

		private TestSQLDataAccessImpl(Connection connection) {
			super(null);
			this.connection = connection;
		}

		@Override
		Connection getConnection() {
			return connection;
		}

		@Override
		SkyveDialect getDialect() {
			return null;
		}
	}

	private static final class FailingSQLDataAccessImpl extends SQLDataAccessImpl {
		private final DomainException exception;

		private FailingSQLDataAccessImpl(DomainException exception) {
			super(null);
			this.exception = exception;
		}

		@Override
		Connection getConnection() {
			throw exception;
		}
	}
}
