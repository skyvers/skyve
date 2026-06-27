package org.skyve.impl.dataaccess.sql;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.SQLException;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.AbstractSQL;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.SQLDefinition;
import org.skyve.metadata.user.User;

@SuppressWarnings({ "static-method", "resource", "boxing", "unchecked" })
class SQLDataAccessImplTest {
	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

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

	@Test
	void newSQLWithModuleAndDocumentLooksUpDrivingDocument() throws Exception {
		Fixture fixture = fixture("select * from orders", 15);
		bindPersistenceWithUser(fixture.user);
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);

		AbstractSQL sql = (AbstractSQL) dataAccess.newSQL("sales", "Order", "select * from orders");

		assertInstanceOf(SQLDataAccessSQL.class, sql);
		assertEquals("select * from orders", sql.toQueryString());
		assertEquals("sales", sql.getModuleName());
		assertEquals("Order", sql.getDocumentName());
		verify(fixture.module).getDocument(fixture.customer, "Order");
	}

	@Test
	void newNamedSQLWithDocumentUsesModuleQueryAndTimeout() throws Exception {
		Fixture fixture = fixture("select * from orders where status = :status", 20);
		bindPersistenceWithUser(fixture.user);
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);

		AbstractSQL sql = (AbstractSQL) dataAccess.newNamedSQL(fixture.document, "OpenOrders");

		assertInstanceOf(SQLDataAccessSQL.class, sql);
		assertEquals("select * from orders where status = :status", sql.toQueryString());
		assertEquals("sales", sql.getModuleName());
		assertEquals("Order", sql.getDocumentName());
		assertEquals(20, sql.getTimeoutInSeconds());
		verify(fixture.module).getSQL("OpenOrders");
	}

	@Test
	void newNamedSQLWithModuleAndDocumentUsesModuleQueryAndTimeout() throws Exception {
		Fixture fixture = fixture("select * from archived_orders", 30);
		bindPersistenceWithUser(fixture.user);
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);

		AbstractSQL sql = (AbstractSQL) dataAccess.newNamedSQL("sales", "Order", "ArchivedOrders");

		assertInstanceOf(SQLDataAccessSQL.class, sql);
		assertEquals("select * from archived_orders", sql.toQueryString());
		assertEquals("sales", sql.getModuleName());
		assertEquals("Order", sql.getDocumentName());
		assertEquals(30, sql.getTimeoutInSeconds());
		verify(fixture.module).getSQL("ArchivedOrders");
		verify(fixture.module).getDocument(fixture.customer, "Order");
	}

	@Test
	void newNamedSQLWithModuleOnlyUsesModuleQueryAndTimeout() throws Exception {
		Fixture fixture = fixture("select count(*) from orders", 45);
		bindPersistenceWithUser(fixture.user);
		SQLDataAccessImpl dataAccess = new SQLDataAccessImpl(null);

		AbstractSQL sql = (AbstractSQL) dataAccess.newNamedSQL("sales", "OrderCount");

		assertInstanceOf(SQLDataAccessSQL.class, sql);
		assertEquals("select count(*) from orders", sql.toQueryString());
		assertEquals(45, sql.getTimeoutInSeconds());
		verify(fixture.module).getSQL("OrderCount");
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

	private static Fixture fixture(String query, int timeoutInSeconds) {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		SQLDefinition sqlDefinition = mock(SQLDefinition.class);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		when(module.getSQL("OpenOrders")).thenReturn(sqlDefinition);
		when(module.getSQL("ArchivedOrders")).thenReturn(sqlDefinition);
		when(module.getSQL("OrderCount")).thenReturn(sqlDefinition);
		when(document.getOwningModuleName()).thenReturn("sales");
		when(document.getName()).thenReturn("Order");
		when(sqlDefinition.getQuery()).thenReturn(query);
		when(sqlDefinition.getTimeoutInSeconds()).thenReturn(timeoutInSeconds);

		return new Fixture(user, customer, module, document, sqlDefinition);
	}

	private static void bindPersistenceWithUser(User user) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}

	private record Fixture(User user, Customer customer, Module module, Document document, SQLDefinition sqlDefinition) {
		// fixture tuple
	}
}
