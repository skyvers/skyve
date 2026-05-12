package org.skyve.impl.persistence.hibernate;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;

import org.hibernate.Session;
import org.hibernate.query.Query;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;

/**
 * Tests for {@link HibernateDocumentQuery}.
 *
 * Pass 1 covers {@link HibernateQueryDelegate} which HibernateDocumentQuery delegates to
 * (constructors require full Skyve metadata context via AbstractPersistence.get()).
 *
 * Pass 2 constructs HibernateDocumentQuery with a mocked Document and a thread-bound
 * TestHibernatePersistence, then executes the generated HQL against the bootstrapped H2
 * session factory.
 */
@SuppressWarnings("static-method")
class HibernateDocumentQueryTest {

	@BeforeAll
	static void setupHibernateBootstrap() throws Exception {
		AbstractHibernatePersistenceTest.setupPersistenceBootstrap();
	}

	@AfterAll
	static void tearDownHibernateBootstrap() throws Exception {
		AbstractHibernatePersistenceTest.tearDownPersistenceBootstrap();
	}

	// -----------------------------------------------------------------------
	// Pass 1 — HibernateQueryDelegate coverage (mock session)
	// -----------------------------------------------------------------------

	@Test
	@SuppressWarnings({"deprecation", "resource"})
	void testQueryDelegateBeanResultsWithBeanAlias() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			injectSession(persistence, session);

			Query<Object> q = mock(Query.class);
			when(session.createQuery(anyString())).thenReturn(q);
			when(q.setFetchSize(anyInt())).thenReturn(q);
			when(q.getReturnAliases()).thenReturn(new String[] {"bean"});
			when(q.list()).thenReturn(List.of());

			HibernateQueryDelegate delegate = new HibernateQueryDelegate(persistence);
			List<?> results = delegate.list(q, true, true, false);

			assertNotNull(results);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	@SuppressWarnings({"deprecation", "resource"})
	void testQueryDelegateScalarResultsPath() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			Session session = mock(Session.class);
			injectSession(persistence, session);

			Query<Object> q = mock(Query.class);
			when(session.createQuery(anyString())).thenReturn(q);
			when(q.setFetchSize(anyInt())).thenReturn(q);
			when(q.getReturnAliases()).thenReturn(new String[] {"count"});
			when(q.list()).thenReturn(List.of());

			HibernateQueryDelegate delegate = new HibernateQueryDelegate(persistence);
			List<?> results = delegate.list(q, false, false, false);

			assertNotNull(results);
		}
		finally {
			persistence.close();
		}
	}

	// -----------------------------------------------------------------------
	// Pass 2 — HibernateDocumentQuery constructor + real H2 execution
	// -----------------------------------------------------------------------

	@Test
	void constructorFromDocumentCreatesInstance() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = newUserBoundPersistence();
		try {
			HibernateDocumentQuery dq = newDocumentQuery(p);
			assertNotNull(dq);
		}
		finally {
			AbstractHibernatePersistenceTest.unbindPersistenceFromThread();
			p.close();
		}
	}

	@Test
	void setFirstResultReturnsSameDocumentQuery() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = newUserBoundPersistence();
		try {
			HibernateDocumentQuery dq = newDocumentQuery(p);
			DocumentQuery result = dq.setFirstResult(5);
			assertSame(dq, result);
		}
		finally {
			AbstractHibernatePersistenceTest.unbindPersistenceFromThread();
			p.close();
		}
	}

	@Test
	void setMaxResultsReturnsSameDocumentQuery() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = newUserBoundPersistence();
		try {
			HibernateDocumentQuery dq = newDocumentQuery(p);
			DocumentQuery result = dq.setMaxResults(20);
			assertSame(dq, result);
		}
		finally {
			AbstractHibernatePersistenceTest.unbindPersistenceFromThread();
			p.close();
		}
	}

	@Test
	void beanResultsExecutesAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = newUserBoundPersistence();
		try {
			p.begin();
			HibernateDocumentQuery dq = newDocumentQuery(p);
			List<?> results = dq.beanResults();
			assertNotNull(results);
			assertTrue(results.isEmpty());
		}
		finally {
			p.rollback();
			AbstractHibernatePersistenceTest.unbindPersistenceFromThread();
			p.close();
		}
	}

	@Test
	void projectedResultsExecutesAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = newUserBoundPersistence();
		try {
			p.begin();
			HibernateDocumentQuery dq = newDocumentQuery(p);
			List<?> results = dq.projectedResults();
			assertNotNull(results);
			assertTrue(results.isEmpty());
		}
		finally {
			p.rollback();
			AbstractHibernatePersistenceTest.unbindPersistenceFromThread();
			p.close();
		}
	}

	@Test
	void scalarResultsExecutesAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = newUserBoundPersistence();
		try {
			p.begin();
			// single-column projection → scalarResults assertSingle check passes
			HibernateDocumentQuery dq = newDocumentQuery(p);
			dq.addBoundProjection("bean", "bizId", "id");
			List<?> results = dq.scalarResults(String.class);
			assertNotNull(results);
			assertTrue(results.isEmpty());
		}
		finally {
			p.rollback();
			AbstractHibernatePersistenceTest.unbindPersistenceFromThread();
			p.close();
		}
	}

	@Test
	void tupleResultsExecutesAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = newUserBoundPersistence();
		try {
			p.begin();
			// two-column projection → tupleResults assertMultiple check passes
			HibernateDocumentQuery dq = newDocumentQuery(p);
			dq.addBoundProjection("bean", "bizId", "a");
			dq.addBoundProjection("bean", "bizKey", "b");
			List<?> results = dq.tupleResults();
			assertNotNull(results);
			assertTrue(results.isEmpty());
		}
		finally {
			p.rollback();
			AbstractHibernatePersistenceTest.unbindPersistenceFromThread();
			p.close();
		}
	}

	@Test
	void beanIterableScrollsAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = newUserBoundPersistence();
		try {
			p.begin();
			HibernateDocumentQuery dq = newDocumentQuery(p);
			try (AutoClosingIterable<?> iter = dq.beanIterable()) {
				assertNotNull(iter);
			}
		}
		finally {
			p.rollback();
			AbstractHibernatePersistenceTest.unbindPersistenceFromThread();
			p.close();
		}
	}

	@Test
	void projectedIterableScrollsAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = newUserBoundPersistence();
		try {
			p.begin();
			HibernateDocumentQuery dq = newDocumentQuery(p);
			try (AutoClosingIterable<?> iter = dq.projectedIterable()) {
				assertNotNull(iter);
			}
		}
		finally {
			p.rollback();
			AbstractHibernatePersistenceTest.unbindPersistenceFromThread();
			p.close();
		}
	}

	@Test
	void scalarIterableScrollsAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = newUserBoundPersistence();
		try {
			p.begin();
			HibernateDocumentQuery dq = newDocumentQuery(p);
			dq.addBoundProjection("bean", "bizId", "id");
			try (AutoClosingIterable<String> iter = dq.scalarIterable(String.class)) {
				assertNotNull(iter);
			}
		}
		finally {
			p.rollback();
			AbstractHibernatePersistenceTest.unbindPersistenceFromThread();
			p.close();
		}
	}

	@Test
	void tupleIterableScrollsAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = newUserBoundPersistence();
		try {
			p.begin();
			HibernateDocumentQuery dq = newDocumentQuery(p);
			dq.addBoundProjection("bean", "bizId", "a");
			dq.addBoundProjection("bean", "bizKey", "b");
			try (AutoClosingIterable<Object[]> iter = dq.tupleIterable()) {
				assertNotNull(iter);
			}
		}
		finally {
			p.rollback();
			AbstractHibernatePersistenceTest.unbindPersistenceFromThread();
			p.close();
		}
	}

	// -----------------------------------------------------------------------
	// Helpers
	// -----------------------------------------------------------------------

	/**
	 * Create a TestHibernatePersistence with a test user set and bound to the
	 * current thread so that {@link org.skyve.impl.persistence.AbstractPersistence#get()}
	 * returns it during DocumentQuery construction.
	 */
	private static AbstractHibernatePersistenceTest.TestHibernatePersistence newUserBoundPersistence()
			throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p =
				new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		p.setUser(AbstractHibernatePersistenceTest.createTestUser());
		AbstractHibernatePersistenceTest.bindPersistenceToThread(p);
		return p;
	}

	/**
	 * Build a {@link HibernateDocumentQuery} for the bootstrapped {@code adminContact}
	 * entity using a mocked {@link Document}.
	 */
	private static HibernateDocumentQuery newDocumentQuery(
			AbstractHibernatePersistenceTest.TestHibernatePersistence persistence) {
		Document doc = mock(Document.class);
		when(doc.getOwningModuleName()).thenReturn("admin");
		when(doc.getName()).thenReturn("Contact");
		return new HibernateDocumentQuery(doc, persistence);
	}

	private static void injectSession(
			AbstractHibernatePersistenceTest.TestHibernatePersistence persistence, Session session) {
		try {
			java.lang.reflect.Field f = AbstractHibernatePersistence.class.getDeclaredField("session");
			f.setAccessible(true);
			f.set(persistence, session);
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not inject mock session", e);
		}
	}
}
