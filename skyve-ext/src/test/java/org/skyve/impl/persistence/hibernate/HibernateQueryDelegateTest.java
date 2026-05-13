package org.skyve.impl.persistence.hibernate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Set;

import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.Session;
import org.hibernate.query.Query;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.TimeoutException;
import org.skyve.impl.persistence.AbstractQuery;
import org.skyve.persistence.DataStore;

import jakarta.persistence.QueryTimeoutException;

@SuppressWarnings("static-method")
class HibernateQueryDelegateTest {

	@BeforeAll
	static void setupHibernateBootstrap() throws Exception {
		AbstractHibernatePersistenceTest.setupPersistenceBootstrap();
	}

	@AfterAll
	static void tearDownHibernateBootstrap() throws Exception {
		AbstractHibernatePersistenceTest.tearDownPersistenceBootstrap();
	}

	@Test
	void testTimeoutQueryUsesAsyncConnectionTimeoutWhenDefaultTimeoutAndAsyncThread() {
		DataStore original = org.skyve.impl.util.UtilImpl.DATA_STORE;
		try {
			org.skyve.impl.util.UtilImpl.DATA_STORE = new DataStore("org.h2.Driver",
					"jdbc:h2:mem:hqd_async;DB_CLOSE_DELAY=-1",
					"sa",
					"",
					"org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect",
					3,
					17);
			Query<?> query = mock(Query.class);

			HibernateQueryDelegate.timeoutQuery(query, 0, true);

			verify(query).setTimeout(17);
		}
		finally {
			org.skyve.impl.util.UtilImpl.DATA_STORE = original;
		}
	}

	@Test
	void testTimeoutQueryUsesOltpConnectionTimeoutWhenDefaultTimeoutAndOltpThread() {
		DataStore original = org.skyve.impl.util.UtilImpl.DATA_STORE;
		try {
			org.skyve.impl.util.UtilImpl.DATA_STORE = new DataStore("org.h2.Driver",
					"jdbc:h2:mem:hqd_oltp;DB_CLOSE_DELAY=-1",
					"sa",
					"",
					"org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect",
					9,
					0);
			Query<?> query = mock(Query.class);

			HibernateQueryDelegate.timeoutQuery(query, 0, false);

			verify(query).setTimeout(9);
		}
		finally {
			org.skyve.impl.util.UtilImpl.DATA_STORE = original;
		}
	}

	@Test
	void testTimeoutQueryUsesExplicitTimeoutWhenProvided() {
		Query<?> query = mock(Query.class);

		HibernateQueryDelegate.timeoutQuery(query, 4, false);

		verify(query).setTimeout(4);
	}

	@Test
	@SuppressWarnings("deprecation")
	void testListAsIsThrowsWhenSingleProjectionExpectedButMultipleProvided() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateQueryDelegate delegate = new HibernateQueryDelegate(persistence);
			Query<Object[]> query = mock(Query.class);
			when(query.getReturnAliases()).thenReturn(new String[] {"a", "b"});

			assertThrows(DomainException.class, () -> delegate.list(query, true, true, false));
		}
		finally {
			persistence.close();
		}
	}

	@Test
	@SuppressWarnings("deprecation")
	void testListProjectedCreatesDynamicBeans() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateQueryDelegate delegate = new HibernateQueryDelegate(persistence);
			setField(delegate, "drivingModuleName", "admin");
			setField(delegate, "drivingDocumentName", "Contact");

			Query<Object> query = mock(Query.class);
			when(query.getReturnAliases()).thenReturn(new String[] {"name", "age"});
			when(query.list()).thenReturn(List.of((Object) new Object[] {"Bob", Integer.valueOf(42)}));

			List<Object> results = delegate.list(query, false, false, false);

			assertEquals(1, results.size());
			assertInstanceOf(DynamicBean.class, results.get(0));
			DynamicBean bean = (DynamicBean) results.get(0);
			assertEquals("admin", bean.getBizModule());
			assertEquals("Contact", bean.getBizDocument());
			assertEquals("Bob", bean.get("name"));
			assertEquals(Integer.valueOf(42), bean.get("age"));
		}
		finally {
			persistence.close();
		}
	}

	@Test
	@SuppressWarnings("deprecation")
	void testListMapsQueryTimeoutExceptionToTimeoutException() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateQueryDelegate delegate = new HibernateQueryDelegate(persistence);
			Query<Object> query = mock(Query.class);
			when(query.getReturnAliases()).thenThrow(new QueryTimeoutException("timeout"));

			assertThrows(TimeoutException.class, () -> delegate.list(query, true, false, false));
		}
		finally {
			persistence.close();
		}
	}

	@Test
	@SuppressWarnings({"resource", "boxing", "deprecation"})
	void testIterateAsIsReturnsIterableOverScrollableResults() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateQueryDelegate delegate = new HibernateQueryDelegate(persistence);
			Query<Object> query = mock(Query.class);
			ScrollableResults scroll = mock(ScrollableResults.class);

			when(query.getReturnAliases()).thenReturn(new String[] {"value"});
			when(query.scroll(ScrollMode.FORWARD_ONLY)).thenReturn(scroll);
			when(scroll.next()).thenReturn(true, false);
			when(scroll.get()).thenReturn(new Object[] {"row-1"});

			var iterable = delegate.iterate(query, true, true, false);
			var iterator = iterable.iterator();
			assertTrue(iterator.hasNext());
			assertEquals("row-1", iterator.next());
			assertFalse(iterator.hasNext());
			verify(scroll).close();
		}
		finally {
			persistence.close();
		}
	}

	@Test
	@SuppressWarnings({"resource", "boxing"})
	void testExecuteBindsCollectionArrayAndScalarParameters() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			setPersistenceSession(persistence, session);

			Query<?> hibernateQuery = mock(Query.class);
			when(session.createQuery("update Something set x = :x where id in (:ids) and code in (:codes)"))
					.thenReturn(hibernateQuery);
			when(hibernateQuery.executeUpdate()).thenReturn(Integer.valueOf(7));

			TestQuery query = new TestQuery("update Something set x = :x where id in (:ids) and code in (:codes)");
			query.setTimeoutInSeconds(2);
			query.addParameter("ids", List.of("a", "b"));
			query.addParameter("codes", new String[] {"C1", "C2"});
			query.addParameter("x", Integer.valueOf(9));

			HibernateQueryDelegate delegate = new HibernateQueryDelegate(persistence);
			int updated = delegate.execute(query);

			assertEquals(7, updated);
			verify(hibernateQuery).setTimeout(2);
			verify(hibernateQuery).setParameterList(eq("ids"), eq(List.of("a", "b")));
			verify(hibernateQuery).setParameterList(eq("codes"), any(Object[].class));
			verify(hibernateQuery).setParameter("x", Integer.valueOf(9));
		}
		finally {
			persistence.close();
		}
	}

	/** Covers list(asIs=true, assertMultiple=true) error when only 1 alias returned. */
	@Test
	@SuppressWarnings("deprecation")
	void testListAsIsThrowsWhenMultipleProjectionsExpectedButSingleProvided() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateQueryDelegate delegate = new HibernateQueryDelegate(persistence);
			Query<Object> query = mock(Query.class);
			when(query.getReturnAliases()).thenReturn(new String[] {"only_one"});

			assertThrows(DomainException.class, () -> delegate.list(query, true, false, true));
		}
		finally {
			persistence.close();
		}
	}

	/** Covers list(asIs=false) DynamicBean path when result is a single non-array value. */
	@Test
	@SuppressWarnings("deprecation")
	void testListProjectedCreatesDynamicBeanWithSingleNonArrayResult() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateQueryDelegate delegate = new HibernateQueryDelegate(persistence);
			setField(delegate, "drivingModuleName", "admin");
			setField(delegate, "drivingDocumentName", "Contact");

			Query<Object> query = mock(Query.class);
			when(query.getReturnAliases()).thenReturn(new String[] {"name"});
			when(query.list()).thenReturn(List.of((Object) "Alice"));

			List<Object> results = delegate.list(query, false, false, false);

			assertEquals(1, results.size());
			assertInstanceOf(org.skyve.domain.DynamicBean.class, results.get(0));
			org.skyve.domain.DynamicBean bean = (org.skyve.domain.DynamicBean) results.get(0);
			assertEquals("Alice", bean.get("name"));
		}
		finally {
			persistence.close();
		}
	}

	/** Covers iterate(asIs=true, assertSingle=true) error when multiple aliases returned. */
	@Test
	@SuppressWarnings("deprecation")
	void testIterateThrowsWhenSingleProjectionExpectedButMultipleProvided() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateQueryDelegate delegate = new HibernateQueryDelegate(persistence);
			Query<Object> query = mock(Query.class);
			when(query.getReturnAliases()).thenReturn(new String[] {"a", "b"});

			assertThrows(DomainException.class, () -> delegate.iterate(query, true, true, false));
		}
		finally {
			persistence.close();
		}
	}

	/** Covers iterate(asIs=true, assertMultiple=true) error when only 1 alias returned. */
	@Test
	@SuppressWarnings("deprecation")
	void testIterateThrowsWhenMultipleProjectionsExpectedButSingleProvided() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateQueryDelegate delegate = new HibernateQueryDelegate(persistence);
			Query<Object> query = mock(Query.class);
			when(query.getReturnAliases()).thenReturn(new String[] {"only_one"});

			assertThrows(DomainException.class, () -> delegate.iterate(query, true, false, true));
		}
		finally {
			persistence.close();
		}
	}

	@Test
	@SuppressWarnings({"resource", "boxing", "deprecation"})
	void testIterateProjectedCreatesDynamicBeanRows() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateQueryDelegate delegate = new HibernateQueryDelegate(persistence);
			setField(delegate, "drivingModuleName", "admin");
			setField(delegate, "drivingDocumentName", "Contact");
			Query<Object[]> query = mock(Query.class);
			ScrollableResults scroll = mock(ScrollableResults.class);

			when(query.getReturnAliases()).thenReturn(new String[] {"name", "status"});
			when(query.scroll(ScrollMode.FORWARD_ONLY)).thenReturn(scroll);
			when(scroll.next()).thenReturn(true, false);
			when(scroll.get()).thenReturn(new Object[] {"Alice", "A"});

			var iterable = delegate.iterate(query, false, false, false);
			var iterator = iterable.iterator();
			assertTrue(iterator.hasNext());
			Object row = iterator.next();
			assertInstanceOf(DynamicBean.class, row);
			DynamicBean bean = (DynamicBean) row;
			assertEquals("admin", bean.getBizModule());
			assertEquals("Contact", bean.getBizDocument());
			assertEquals("Alice", bean.get("name"));
			assertEquals("A", bean.get("status"));
			assertFalse(iterator.hasNext());
		}
		finally {
			persistence.close();
		}
	}

	private static void setField(Object target, String fieldName, Object value) throws Exception {
		Field field = HibernateQueryDelegate.class.getDeclaredField(fieldName);
		field.setAccessible(true);
		field.set(target, value);
	}

	private static void setPersistenceSession(AbstractHibernatePersistence persistence, Session session) {
		try {
			Field sessionField = AbstractHibernatePersistence.class.getDeclaredField("session");
			sessionField.setAccessible(true);
			sessionField.set(persistence, session);
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not set test session", e);
		}
	}

	private static final class TestQuery extends AbstractQuery {
		private final String queryString;

		private TestQuery(String queryString) {
			this.queryString = queryString;
		}

		@Override
		public String toQueryString() {
			return queryString;
		}

		private void addParameter(String name, Object value) {
			parameters.put(name, value);
		}

		@SuppressWarnings("unused")
		private Set<String> names() {
			return getParameterNames();
		}
	}
}
