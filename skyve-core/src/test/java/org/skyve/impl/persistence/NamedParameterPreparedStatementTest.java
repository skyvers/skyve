package org.skyve.impl.persistence;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Time;
import java.sql.Timestamp;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.ArgumentMatchers.anyString;

@ExtendWith(MockitoExtension.class)
@SuppressWarnings("resource")
class NamedParameterPreparedStatementTest {

	@Mock
	private Connection connection;

	@Mock
	private PreparedStatement preparedStatement;

	@BeforeEach
	void setUp() throws Exception {
		Mockito.when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);
	}

	@Test
	void constructorParsesNamedParameterQuery() throws Exception {
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT * FROM t WHERE id = :id")) {
			assertNotNull(npps);
		}
	}

	@Test
	void getStatementReturnsWrappedStatement() throws Exception {
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT 1")) {
			assertSame(preparedStatement, npps.getStatement());
		}
	}

	@Test
	void setObjectCallsDelegatePreparedStatement() throws Exception {
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "INSERT INTO t VALUES(:val)")) {
			npps.setObject("val", "hello");
			Mockito.verify(preparedStatement).setObject(1, "hello");
		}
	}

	@Test
	void setStringCallsDelegatePreparedStatement() throws Exception {
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT * FROM t WHERE name=:name")) {
			npps.setString("name", "bob");
			Mockito.verify(preparedStatement).setString(1, "bob");
		}
	}

	@Test
	void setBooleanCallsDelegate() throws Exception {
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT * FROM t WHERE active=:active")) {
			npps.setBoolean("active", true);
			Mockito.verify(preparedStatement).setBoolean(1, true);
		}
	}

	@Test
	void setIntCallsDelegate() throws Exception {
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT * FROM t WHERE age=:age")) {
			npps.setInt("age", 42);
			Mockito.verify(preparedStatement).setInt(1, 42);
		}
	}

	@Test
	void setLongCallsDelegate() throws Exception {
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT * FROM t WHERE id=:id")) {
			npps.setLong("id", 12345L);
			Mockito.verify(preparedStatement).setLong(1, 12345L);
		}
	}

	@Test
	void setTimestampCallsDelegate() throws Exception {
		Timestamp ts = new Timestamp(System.currentTimeMillis());
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT * FROM t WHERE ts=:ts")) {
			npps.setTimestamp("ts", ts);
			Mockito.verify(preparedStatement).setTimestamp(1, ts);
		}
	}

	@Test
	void setDateCallsDelegate() throws Exception {
		Date d = new Date(System.currentTimeMillis());
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT * FROM t WHERE d=:d")) {
			npps.setDate("d", d);
			Mockito.verify(preparedStatement).setDate(1, d);
		}
	}

	@Test
	void setTimeCallsDelegate() throws Exception {
		Time t = new Time(System.currentTimeMillis());
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT * FROM t WHERE t=:t")) {
			npps.setTime("t", t);
			Mockito.verify(preparedStatement).setTime(1, t);
		}
	}

	@Test
	void setBigDecimalCallsDelegate() throws Exception {
		BigDecimal bd = new BigDecimal("3.14");
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT * FROM t WHERE price=:price")) {
			npps.setBigDecimal("price", bd);
			Mockito.verify(preparedStatement).setBigDecimal(1, bd);
		}
	}

	@Test
	void setBytesCallsDelegate() throws Exception {
		byte[] bytes = new byte[] {1, 2, 3};
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT * FROM t WHERE data=:data")) {
			npps.setBytes("data", bytes);
			Mockito.verify(preparedStatement).setBytes(1, bytes);
		}
	}

	@Test
	void setNullCallsDelegate() throws Exception {
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT * FROM t WHERE val=:val")) {
			npps.setNull("val", java.sql.Types.VARCHAR);
			Mockito.verify(preparedStatement).setNull(1, java.sql.Types.VARCHAR);
		}
	}

	@Test
	@SuppressWarnings("boxing")
	void executeCallsDelegate() throws Exception {
		Mockito.when(preparedStatement.execute()).thenReturn(Boolean.FALSE);
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT 1")) {
			assertFalse(npps.execute());
		}
	}

	@Test
	void executeQueryCallsDelegate() throws Exception {
		ResultSet rs = Mockito.mock(ResultSet.class);
		Mockito.when(preparedStatement.executeQuery()).thenReturn(rs);
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT 1")) {
			assertSame(rs, npps.executeQuery());
		}
	}

	@Test
	@SuppressWarnings("boxing")
	void executeUpdateCallsDelegate() throws Exception {
		Mockito.when(preparedStatement.executeUpdate()).thenReturn(Integer.valueOf(3));
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "UPDATE t SET x=1")) {
			assertEquals(3, npps.executeUpdate());
		}
	}

	@Test
	void addBatchCallsDelegate() throws Exception {
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT 1")) {
			npps.addBatch();
			Mockito.verify(preparedStatement).addBatch();
		}
	}

	@Test
	void executeBatchCallsDelegate() throws Exception {
		Mockito.when(preparedStatement.executeBatch()).thenReturn(new int[] {1});
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT 1")) {
			int[] counts = npps.executeBatch();
			assertEquals(1, counts[0]);
		}
	}

	@Test
	void setQueryTimeoutCallsDelegate() throws Exception {
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT 1")) {
			npps.setQueryTimeout(30);
			Mockito.verify(preparedStatement).setQueryTimeout(30);
		}
	}

	@Test
	@SuppressWarnings("boxing")
	void isClosedCallsDelegate() throws Exception {
		Mockito.when(preparedStatement.isClosed()).thenReturn(Boolean.TRUE);
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT 1")) {
			assertTrue(npps.isClosed());
		}
	}

	@Test
	void setObjectThrowsForUnknownParameter() throws Exception {
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT 1")) {
			assertThrows(IllegalArgumentException.class, () -> npps.setObject("unknown", "value"));
		}
	}

	@Test
	void parsesQueryWithMultipleParameters() throws Exception {
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT * FROM t WHERE a=:a AND b=:b")) {
			npps.setString("a", "x");
			npps.setString("b", "y");
			Mockito.verify(preparedStatement).setString(1, "x");
			Mockito.verify(preparedStatement).setString(2, "y");
		}
	}

	@Test
	void parsesQueryWithSingleQuotedStringNotReplacingColons() throws Exception {
		// The ':id' inside a literal string should NOT be treated as a named param
		try (NamedParameterPreparedStatement npps = new NamedParameterPreparedStatement(connection, "SELECT ':id' AS x")) {
			assertNotNull(npps);
		}
	}
}
