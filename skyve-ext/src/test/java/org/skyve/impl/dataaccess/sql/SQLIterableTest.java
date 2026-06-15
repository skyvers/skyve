package org.skyve.impl.dataaccess.sql;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
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
import java.sql.Types;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.TimeoutException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractBean;
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
	void constructorRethrowsSkyveExceptionFromSqlPreparation() throws Exception {
		Fixture fixture = fixture(null);
		DomainException expected = new DomainException("prepared badly");
		doThrow(expected).when(fixture.sql).prepareStatement(any(), any(), any());

		DomainException actual = assertThrows(DomainException.class,
				() -> new SQLIterable<>(null, fixture.dataAccess, fixture.sql, null));

		assertSame(expected, actual);
	}

	@Test
	void constructorWrapsSetupException() throws Exception {
		Connection connection = mock(Connection.class);
		when(connection.prepareStatement(any(String.class))).thenThrow(new SQLException("no statement"));
		SQLDataAccessSQL sql = mock(SQLDataAccessSQL.class);
		when(sql.toQueryString()).thenReturn("select 1");
		TestSQLDataAccessImpl dataAccess = new TestSQLDataAccessImpl(connection);

		DomainException e = assertThrows(DomainException.class, () -> new SQLIterable<>(null, dataAccess, sql, null));

		assertEquals("Cannot setup an SQLIterable", e.getMessage());
		assertTrue(e.getCause() instanceof SQLException);
	}

	@Test
	void nextBeanPopulatesTextTemporalAndNullValues() throws Exception {
		User user = bindMockUser();
		Customer customer = user.getCustomer();
		Document document = mock(Document.class);
		TemporalBean bean = new TemporalBean();
		when(document.newInstance(user)).thenReturn(bean);
		doReturn(List.of(
				attribute("description", AttributeType.text),
				attribute("orderDate", AttributeType.date),
				attribute("updatedAt", AttributeType.dateTime),
				attribute("finishTime", AttributeType.time),
				attribute("auditStamp", AttributeType.timestamp),
				attribute("optionalNumber", AttributeType.integer))).when(document).getAllAttributes(customer);
		Fixture fixture = fixture(null);
		java.sql.Date date = java.sql.Date.valueOf("2026-06-10");
		java.sql.Time time = java.sql.Time.valueOf("13:14:15");
		when(fixture.resultSet.getString("description")).thenReturn("packed");
		when(fixture.resultSet.getDate("orderDate")).thenReturn(date);
		when(fixture.resultSet.getTime("updatedAt")).thenReturn(time);
		when(fixture.resultSet.getTime("finishTime")).thenReturn(time);
		when(fixture.resultSet.getDate("auditStamp")).thenReturn(date);
		when(fixture.resultSet.getInt("optionalNumber")).thenReturn(Integer.valueOf(0));
		when(fixture.resultSet.wasNull()).thenReturn(Boolean.TRUE);
		SQLIterable<TemporalBean> iterable = new SQLIterable<>(document, fixture.dataAccess, fixture.sql, null);

		TemporalBean result = iterable.iterator().next();

		assertSame(bean, result);
		assertEquals("packed", result.description);
		assertNotNull(result.orderDate);
		assertNotNull(result.updatedAt);
		assertNotNull(result.finishTime);
		assertNotNull(result.auditStamp);
		assertNull(result.optionalNumber);
	}

	@Test
	void nextBeanPopulatesGeometryUsingDialect() throws Exception {
		User user = bindMockUser();
		Customer customer = user.getCustomer();
		Document document = mock(Document.class);
		Map<String, Object> properties = new TreeMap<>();
		properties.put("shape", null);
		DynamicBean bean = new DynamicBean("sales", "Order", properties);
		when(document.newInstance(user)).thenReturn(bean);
		doReturn(List.of(attribute("shape", AttributeType.geometry))).when(document).getAllAttributes(customer);
		Fixture fixture = fixture(null);
		SkyveDialect dialect = mock(SkyveDialect.class);
		Geometry geometry = mock(Geometry.class);
		when(dialect.getGeometrySqlType()).thenReturn(Integer.valueOf(Types.OTHER));
		when(dialect.convertFromPersistedValue("persisted")).thenReturn(geometry);
		fixture.dataAccess.setDialect(dialect);
		when(fixture.resultSet.getObject("shape")).thenReturn("persisted");
		SQLIterable<DynamicBean> iterable = new SQLIterable<>(document, fixture.dataAccess, fixture.sql, null);

		DynamicBean result = iterable.iterator().next();

		assertSame(geometry, result.get("shape"));
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

	public static final class TemporalBean extends AbstractBean {
		private static final long serialVersionUID = 1L;

		private String description;
		private java.util.Date orderDate;
		private DateTime updatedAt;
		private TimeOnly finishTime;
		private Timestamp auditStamp;
		private Integer optionalNumber;

		@Override
		public String getBizId() {
			return null;
		}

		@Override
		public String getBizModule() {
			return "sales";
		}

		@Override
		public String getBizDocument() {
			return "Order";
		}

		@Override
		public String getBizCustomer() {
			return null;
		}

		@Override
		public void setBizCustomer(String bizCustomer) {
			// test bean only
		}

		@Override
		public String getBizDataGroupId() {
			return null;
		}

		@Override
		public void setBizDataGroupId(String bizDataGroupId) {
			// test bean only
		}

		@Override
		public String getBizUserId() {
			return null;
		}

		@Override
		public void setBizUserId(String bizUserId) {
			// test bean only
		}

		@Override
		public String getBizKey() {
			return null;
		}

		public String getDescription() {
			return description;
		}

		public void setDescription(String description) {
			this.description = description;
		}

		public java.util.Date getOrderDate() {
			return orderDate;
		}

		public void setOrderDate(java.util.Date orderDate) {
			this.orderDate = orderDate;
		}

		public DateTime getUpdatedAt() {
			return updatedAt;
		}

		public void setUpdatedAt(DateTime updatedAt) {
			this.updatedAt = updatedAt;
		}

		public TimeOnly getFinishTime() {
			return finishTime;
		}

		public void setFinishTime(TimeOnly finishTime) {
			this.finishTime = finishTime;
		}

		public Timestamp getAuditStamp() {
			return auditStamp;
		}

		public void setAuditStamp(Timestamp auditStamp) {
			this.auditStamp = auditStamp;
		}

		public Integer getOptionalNumber() {
			return optionalNumber;
		}

		public void setOptionalNumber(Integer optionalNumber) {
			this.optionalNumber = optionalNumber;
		}
	}

	private static final class TestSQLDataAccessImpl extends SQLDataAccessImpl {
		private final Connection connection;
		private SkyveDialect dialect;

		private TestSQLDataAccessImpl(Connection connection) {
			super(null);
			this.connection = connection;
		}

		void setDialect(SkyveDialect dialect) {
			this.dialect = dialect;
		}

		@Override
		Connection getConnection() {
			return connection;
		}

		@Override
		SkyveDialect getDialect() {
			return dialect;
		}
	}
}
