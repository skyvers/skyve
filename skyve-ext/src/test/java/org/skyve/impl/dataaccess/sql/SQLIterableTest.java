package org.skyve.impl.dataaccess.sql;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.SQLTimeoutException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.TimeoutException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;

@SuppressWarnings({ "static-method", "resource", "boxing" })
class SQLIterableTest {
	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void hasNextReturnsTrueWhenResultSetHasRow() throws Exception {
		Fixture fixture = fixture(null);
		when(fixture.resultSet.next()).thenReturn(Boolean.TRUE);
		SQLIterable<Object> iterable = new SQLIterable<>(null, fixture.dataAccess, fixture.sql, null);

		Iterator<Object> iterator = iterable.iterator();
		boolean hasNext = iterator.hasNext();

		assertTrue(hasNext);
	}

	@Test
	void hasNextClosesResultSetWhenExhausted() throws Exception {
		Fixture fixture = fixture(null);
		when(fixture.resultSet.next()).thenReturn(Boolean.FALSE);
		SQLIterable<Object> iterable = new SQLIterable<>(null, fixture.dataAccess, fixture.sql, null);

		Iterator<Object> iterator = iterable.iterator();
		boolean hasNext = iterator.hasNext();

		assertFalse(hasNext);
		verify(fixture.resultSet).close();
	}

	@Test
	void hasNextSqlTimeoutThrowsTimeoutException() throws Exception {
		Fixture fixture = fixture(null);
		when(fixture.resultSet.next()).thenThrow(new SQLTimeoutException("timed out"));
		SQLIterable<Object> iterable = new SQLIterable<>(null, fixture.dataAccess, fixture.sql, null);

		Iterator<Object> iterator = iterable.iterator();
		assertThrows(TimeoutException.class, iterator::hasNext);
	}

	@Test
	void hasNextSqlExceptionThrowsDomainException() throws Exception {
		Fixture fixture = fixture(null);
		when(fixture.resultSet.next()).thenThrow(new SQLException("broken"));
		SQLIterable<Object> iterable = new SQLIterable<>(null, fixture.dataAccess, fixture.sql, null);

		Iterator<Object> iterator = iterable.iterator();
		assertThrows(DomainException.class, iterator::hasNext);
	}

	@Test
	void nextScalarReturnsConvertedValue() throws Exception {
		Fixture fixture = fixture(null);
		when(fixture.resultSet.getObject(1)).thenReturn(Integer.valueOf(7));
		SQLIterable<Integer> iterable = new SQLIterable<>(null, fixture.dataAccess, fixture.sql, Integer.class);

		Iterator<Integer> iterator = iterable.iterator();
		Integer result = iterator.next();

		assertEquals(Integer.valueOf(7), result);
	}

	@Test
	void nextTupleReturnsRowValues() throws Exception {
		ResultSetMetaData metaData = mock(ResultSetMetaData.class);
		Fixture fixture = fixture(metaData);
		when(metaData.getColumnCount()).thenReturn(Integer.valueOf(2));
		when(fixture.resultSet.getObject(1)).thenReturn("a");
		when(fixture.resultSet.getObject(2)).thenReturn(Integer.valueOf(9));
		SQLIterable<Object[]> iterable = new SQLIterable<>(null, fixture.dataAccess, fixture.sql, null);

		Iterator<Object[]> iterator = iterable.iterator();
		Object[] result = iterator.next();

		assertArrayEquals(new Object[] { "a", Integer.valueOf(9) }, result);
	}

	@Test
	void nextTupleWrapsResultSetException() throws Exception {
		ResultSetMetaData metaData = mock(ResultSetMetaData.class);
		Fixture fixture = fixture(metaData);
		when(metaData.getColumnCount()).thenReturn(Integer.valueOf(2));
		when(fixture.resultSet.getObject(1)).thenThrow(new SQLException("tuple broken"));
		SQLIterable<Object[]> iterable = new SQLIterable<>(null, fixture.dataAccess, fixture.sql, null);

		Iterator<Object[]> iterator = iterable.iterator();
		assertThrows(DomainException.class, iterator::next);
	}

	@Test
	void nextBeanPopulatesSupportedAttributeTypes() throws Exception {
		User user = bindMockUser();
		Customer customer = user.getCustomer();
		Document document = mock(Document.class);
		Map<String, Object> properties = new TreeMap<>();
		for (String name : List.of("active", "amount10", "amount2", "amount5", "count", "longCount")) {
			properties.put(name, null);
		}
		DynamicBean bean = new DynamicBean("sales", "Order", properties);
		when(document.newInstance(user)).thenReturn(bean);
		doReturn(List.of(
				attribute("active", AttributeType.bool),
				attribute("amount10", AttributeType.decimal10),
				attribute("amount2", AttributeType.decimal2),
				attribute("amount5", AttributeType.decimal5),
				attribute("count", AttributeType.integer),
				attribute("longCount", AttributeType.longInteger))).when(document).getAllAttributes(customer);
		Fixture fixture = fixture(null);
		when(fixture.resultSet.getBoolean("active")).thenReturn(Boolean.TRUE);
		when(fixture.resultSet.getDouble("amount10")).thenReturn(Double.valueOf(10.125));
		when(fixture.resultSet.getDouble("amount2")).thenReturn(Double.valueOf(2.12));
		when(fixture.resultSet.getDouble("amount5")).thenReturn(Double.valueOf(5.12345));
		when(fixture.resultSet.getInt("count")).thenReturn(Integer.valueOf(7));
		when(fixture.resultSet.getLong("longCount")).thenReturn(Long.valueOf(12L));
		SQLIterable<DynamicBean> iterable = new SQLIterable<>(document, fixture.dataAccess, fixture.sql, null);

		DynamicBean result = iterable.iterator().next();

		assertSame(bean, result);
		assertEquals(Boolean.TRUE, result.get("active"));
	}

	@Test
	void nextScalarWrapsResultSetException() throws Exception {
		Fixture fixture = fixture(null);
		when(fixture.resultSet.getObject(1)).thenThrow(new SQLException("scalar broken"));
		SQLIterable<Integer> iterable = new SQLIterable<>(null, fixture.dataAccess, fixture.sql, Integer.class);

		Iterator<Integer> iterator = iterable.iterator();
		assertThrows(DomainException.class, iterator::next);
	}

	@Test
	void removeThrowsUnsupportedOperationException() throws Exception {
		Fixture fixture = fixture(null);
		SQLIterable<Object> iterable = new SQLIterable<>(null, fixture.dataAccess, fixture.sql, null);

		Iterator<Object> iterator = iterable.iterator();
		assertThrows(UnsupportedOperationException.class, iterator::remove);
	}

	@Test
	void closeWrapsSQLExceptionInDomainException() throws Exception {
		Fixture fixture = fixture(null);
		doThrow(new SQLException("boom")).when(fixture.resultSet).close();
		SQLIterable<Object> iterable = new SQLIterable<>(null, fixture.dataAccess, fixture.sql, null);

		DomainException e = assertThrows(DomainException.class, iterable::close);
		assertEquals("Could not close resources from SQLIterable", e.getMessage());
	}

	private static Fixture fixture(ResultSetMetaData metaData) throws SQLException {
		Connection connection = mock(Connection.class);
		PreparedStatement preparedStatement = mock(PreparedStatement.class);
		ResultSet resultSet = mock(ResultSet.class);
		when(connection.prepareStatement(any(String.class))).thenReturn(preparedStatement);
		when(preparedStatement.executeQuery()).thenReturn(resultSet);
		when(resultSet.isClosed()).thenReturn(Boolean.FALSE);
		if (metaData != null) {
			when(resultSet.getMetaData()).thenReturn(metaData);
		}
		SQLDataAccessSQL sql = mock(SQLDataAccessSQL.class);
		when(sql.toQueryString()).thenReturn("select 1");

		TestSQLDataAccessImpl dataAccess = new TestSQLDataAccessImpl(connection);
		return new Fixture(dataAccess, sql, resultSet);
	}

	private static Attribute attribute(String name, AttributeType type) {
		Attribute attribute = mock(Attribute.class);
		when(attribute.getName()).thenReturn(name);
		when(attribute.getAttributeType()).thenReturn(type);
		return attribute;
	}

	private static User bindMockUser() throws Exception {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
		return user;
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}

	private static final class Fixture {
		private final TestSQLDataAccessImpl dataAccess;
		private final SQLDataAccessSQL sql;
		private final ResultSet resultSet;

		private Fixture(TestSQLDataAccessImpl dataAccess, SQLDataAccessSQL sql, ResultSet resultSet) {
			this.dataAccess = dataAccess;
			this.sql = sql;
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
}
