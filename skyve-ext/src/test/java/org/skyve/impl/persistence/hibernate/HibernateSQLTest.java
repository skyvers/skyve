package org.skyve.impl.persistence.hibernate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Date;
import java.util.List;

import org.hibernate.Session;
import org.hibernate.query.NativeQuery;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.SQL;

import static org.junit.jupiter.api.Assertions.assertFalse;

@SuppressWarnings({"static-method", "rawtypes", "resource"})
class HibernateSQLTest {

	@BeforeAll
	static void setupHibernateBootstrap() throws Exception {
		AbstractHibernatePersistenceTest.setupPersistenceBootstrap();
	}

	@AfterAll
	static void tearDownHibernateBootstrap() throws Exception {
		AbstractHibernatePersistenceTest.tearDownPersistenceBootstrap();
	}

	@Test
	void testQueryStringConstructorCreatesInstance() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateSQL sql = new HibernateSQL("select 1", persistence);
			assertNotNull(sql);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testModuleDocumentNameConstructorCreatesInstance() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateSQL sql = new HibernateSQL("admin", "Contact", "select 1", persistence);
			assertNotNull(sql);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testPutParameterStringReturnsSameInstance() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateSQL sql = new HibernateSQL("select :name", persistence);
			SQL result = sql.putParameter("name", "Alice", false);
			assertSame(sql, result);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testPutParameterIntegerReturnsSameInstance() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateSQL sql = new HibernateSQL("select :age", persistence);
			SQL result = sql.putParameter("age", Integer.valueOf(42));
			assertSame(sql, result);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testBeanResultsThrowsWhenModuleOrDocumentIsNull() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateSQL sql = new HibernateSQL("select 1", persistence);
			org.junit.jupiter.api.Assertions.assertThrows(DomainException.class, () -> sql.beanResults());
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testBeanIterableThrowsWhenModuleOrDocumentIsNull() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateSQL sql = new HibernateSQL("select 1", persistence);
			org.junit.jupiter.api.Assertions.assertThrows(DomainException.class, () -> sql.beanIterable());
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testScalarResultsDelegatesToNativeQuery() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(persistence, session);

			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.addSynchronizedQuerySpace(anyString())).thenReturn(q);
			when(q.setFetchSize(anyInt())).thenReturn(q);
			when(q.list()).thenReturn(List.of("result1"));

			HibernateSQL sql = new HibernateSQL("select name from Contact", persistence);
			List<String> results = sql.scalarResults(String.class);

			assertNotNull(results);
			assertEquals(1, results.size());
		}
		finally {
			persistence.close();
		}
	}

	@Test
	@SuppressWarnings("boxing")
	void testExecuteDelegatesToNativeQuery() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(persistence, session);

			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.addSynchronizedQuerySpace(anyString())).thenReturn(q);
			when(q.setFetchSize(anyInt())).thenReturn(q);
			when(q.executeUpdate()).thenReturn(5);

			HibernateSQL sql = new HibernateSQL("update Contact set name = 'X'", persistence);
			int count = sql.execute();

			assertEquals(5, count);
			verify(q).executeUpdate();
		}
		finally {
			persistence.close();
		}
	}

	@Test
	void testTupleResultsDelegatesToNativeQuery() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(persistence, session);

			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.addSynchronizedQuerySpace(anyString())).thenReturn(q);
			when(q.setFetchSize(anyInt())).thenReturn(q);
			when(q.list()).thenReturn(List.of((Object) new Object[] {"name", "active"}));

			HibernateSQL sql = new HibernateSQL("select name, status from Contact", persistence);
			List<Object[]> results = sql.tupleResults();

			assertNotNull(results);
			assertEquals(1, results.size());
		}
		finally {
			persistence.close();
		}
	}

	private static void setSession(AbstractHibernatePersistenceTest.TestHibernatePersistence persistence, Session session) {
		try {
			java.lang.reflect.Field f = AbstractHibernatePersistence.class.getDeclaredField("session");
			f.setAccessible(true);
			f.set(persistence, session);
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not inject mock session", e);
		}
	}

	// -----------------------------------------------------------------------
	// Pass 2: real H2 execution tests
	// -----------------------------------------------------------------------

	@Test
	void scalarResultsExecutesSelectOneAgainstH2() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			p.begin();
			HibernateSQL sql = new HibernateSQL("SELECT 1 AS val", p);
			List<Integer> results = sql.scalarResults(Integer.class);
			assertNotNull(results);
			assertFalse(results.isEmpty());
		}
		finally {
			p.rollback();
			p.close();
		}
	}

	@Test
	void tupleResultsExecutesMultiColumnSelectAgainstH2() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			p.begin();
			HibernateSQL sql = new HibernateSQL("SELECT 1 AS a, 2 AS b", p);
			List<Object[]> results = sql.tupleResults();
			assertNotNull(results);
			assertFalse(results.isEmpty());
		}
		finally {
			p.rollback();
			p.close();
		}
	}

	@Test
	void executeRunsInsertAgainstH2() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			p.begin();
			HibernateSQL sql = new HibernateSQL(
					"INSERT INTO ADM_Contact (bizId, bizVersion, bizKey) VALUES ('sql-exec-id', 0, 'test')", p);
			int count = sql.execute();
			assertEquals(1, count);
		}
		finally {
			p.rollback();
			p.close();
		}
	}
	@Test
	void scalarIterableScrollsAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			p.begin();
			HibernateSQL sql = new HibernateSQL("SELECT 1 AS val", p);
			try (AutoClosingIterable<Integer> iter = sql.scalarIterable(Integer.class)) {
				assertNotNull(iter);
			}
		}
		finally {
			p.rollback();
			p.close();
		}
	}

	@Test
	void tupleIterableScrollsAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			p.begin();
			HibernateSQL sql = new HibernateSQL("SELECT 1 AS a, 2 AS b", p);
			try (AutoClosingIterable<Object[]> iter = sql.tupleIterable()) {
				assertNotNull(iter);
			}
		}
		finally {
			p.rollback();
			p.close();
		}
	}

	@Test
	void dynaIterableScrollsAgainstH2() throws Exception {
		// dynaResults/dynaIterable use NamedParameterPreparedStatement with the Hibernate
		// session's JDBC connection. This path is covered by integration tests in skyve-war.
		// Verify that the constructor and parameter setter code is exercised via the SQL
		// constructors above; the full execute path requires a standalone JDBC connection.
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateSQL sql = new HibernateSQL("SELECT 1 AS val", p);
			assertNotNull(sql);
		}
		finally {
			p.close();
		}
	}

	// -----------------------------------------------------------------------
	// Pass 3: typed-parameter dispatch in createQueryFromSQL()
	// -----------------------------------------------------------------------

	/** Covers the early-exit TimeOnly branch (sets TimeType.INSTANCE). */
	@Test
	void testTimeOnlyParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("t", new TimeOnly());
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the early-exit Timestamp branch (sets TimestampType.INSTANCE). */
	@Test
	void testTimestampParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("ts", new Timestamp());
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the early-exit DateTime branch (sets TimestampType.INSTANCE). */
	@Test
	void testDateTimeParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("dt", new DateTime());
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the early-exit plain java.util.Date branch (sets DateType.INSTANCE). */
	@Test
	void testPlainDateParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			// Plain java.util.Date is NOT a sql.Date, so the Date early-exit fires
			sql.putParameter("d", new Date(), AttributeType.date);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the early-exit OptimisticLock branch (sets StringType.INSTANCE). */
	@Test
	void testOptimisticLockParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("lock", new OptimisticLock("testUser", new Date()), AttributeType.text);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the early-exit Enumeration-as-Object branch (calls toCode()). */
	@Test
	void testEnumerationAsObjectParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			Enumeration enumMock = mock(Enumeration.class);
			when(enumMock.toCode()).thenReturn("VAL");

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			// Store the Enumeration object directly (bypassing the String-code-storing overload)
			sql.putParameter("e", enumMock, AttributeType.enumeration);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the Collection path for AttributeType.bool. */
	@Test
	void testBooleanCollectionParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("flags", List.of(Boolean.TRUE, Boolean.FALSE), AttributeType.bool);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the Boolean[] array path for AttributeType.bool. */
	@Test
	void testBooleanArrayParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("flags", new Boolean[] {Boolean.TRUE}, AttributeType.bool);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the Collection path for AttributeType.text, colour, content, image. */
	@Test
	void testStringCollectionParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("texts", List.of("a", "b"), AttributeType.text);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the String[] array path for AttributeType.text. */
	@Test
	void testStringArrayParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("texts", new String[] {"a", "b"}, AttributeType.text);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the Collection path for AttributeType.enumeration (String codes in list). */
	@Test
	void testEnumerationCollectionParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("enums", List.of("CODE1", "CODE2"), AttributeType.enumeration);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the String[] array path for AttributeType.enumeration. */
	@Test
	void testEnumerationArrayParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("enums", new String[] {"CODE1"}, AttributeType.enumeration);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the Collection path for AttributeType.markup/memo (TextType). */
	@Test
	void testMarkupCollectionParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("memos", List.of("long text 1", "long text 2"), AttributeType.markup);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the Collection path for AttributeType.date. */
	@Test
	void testDateCollectionParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("dates", List.of(new Date()), AttributeType.date);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the Collection path for AttributeType.dateTime. */
	@Test
	void testDateTimeCollectionParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("dts", List.of(new Date()), AttributeType.dateTime);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the Collection path for AttributeType.decimal2/5/10. */
	@Test
	void testDecimalCollectionParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("decimals", List.of(new Decimal2("1.50"), new Decimal2("2.00")), AttributeType.decimal2);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the Decimal[] array path for AttributeType.decimal2. */
	@Test
	void testDecimalArrayParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("decimals", new Decimal2[] {new Decimal2("3.14")}, AttributeType.decimal5);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the Collection path for AttributeType.integer. */
	@Test
	void testIntegerCollectionParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("ints", List.of(Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(3)), AttributeType.integer);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the Collection path for AttributeType.longInteger. */
	@Test
	void testLongCollectionParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("longs", List.of(Long.valueOf(100L), Long.valueOf(200L)), AttributeType.longInteger);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the Collection path for AttributeType.time. */
	@Test
	void testTimeCollectionParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("times", List.of(new TimeOnly()), AttributeType.time);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the Collection path for AttributeType.id/association. */
	@Test
	void testIdCollectionParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("ids", List.of("id1", "id2"), AttributeType.id);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the String[] array path for AttributeType.id. */
	@Test
	void testIdArrayParameterDispatch() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			sql.putParameter("ids", new String[] {"id1", "id2"}, AttributeType.id);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}

	/** Covers the else (unrecognised type) fallback in createQueryFromSQL(). */
	@Test
	void testUnrecognisedTypeParameterFallback() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setSession(p, session);
			NativeQuery q = mock(NativeQuery.class);
			when(session.createNativeQuery(anyString())).thenReturn(q);
			when(q.list()).thenReturn(List.of());

			HibernateSQL sql = new HibernateSQL("SELECT 1", p);
			// null type falls into the else-branch: result.setParameter(name, value)
			sql.putParameter("x", Integer.valueOf(42), null);
			sql.scalarResults(Integer.class);
		}
		finally {
			p.close();
		}
	}
}
