package org.skyve.impl.persistence.hibernate;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.util.List;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractBizQL;
import org.skyve.impl.persistence.AbstractQuery;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.BizQL;

/**
 * Tests for {@link HibernateBizQL}.
 *
 * Pass 1 tests cover the constructor and fluent API without DB execution.
 * Pass 2 tests pre-resolve the HQL query string (bypassing the {module.Document}
 * resolution which needs full Skyve customer context) and execute against the
 * bootstrapped H2 session factory.
 */
class HibernateBizQLTest {

	@BeforeAll
	static void setupHibernateBootstrap() throws Exception {
		AbstractHibernatePersistenceTest.setupPersistenceBootstrap();
	}

	@AfterAll
	static void tearDownHibernateBootstrap() throws Exception {
		AbstractHibernatePersistenceTest.tearDownPersistenceBootstrap();
	}

	// -----------------------------------------------------------------------
	// Pass 1 — constructor and fluent API
	// -----------------------------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void testConstructorCreatesInstance() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateBizQL bizQL = new HibernateBizQL("from admin.Contact", persistence);
			assertNotNull(bizQL);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetFirstResultReturnsSameInstance() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateBizQL bizQL = new HibernateBizQL("from admin.Contact", persistence);
			BizQL result = bizQL.setFirstResult(5);
			assertSame(bizQL, result);
		}
		finally {
			persistence.close();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetMaxResultsReturnsSameInstance() {
		AbstractHibernatePersistenceTest.TestHibernatePersistence persistence = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			HibernateBizQL bizQL = new HibernateBizQL("from admin.Contact", persistence);
			BizQL result = bizQL.setMaxResults(10);
			assertSame(bizQL, result);
		}
		finally {
			persistence.close();
		}
	}

	// -----------------------------------------------------------------------
	// Pass 2 — real H2 execution (pre-resolved HQL, empty ADM_Contact table)
	// -----------------------------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void beanResultsExecutesHqlAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			p.begin();
			HibernateBizQL bql = newPreResolvedBizQL("select bean as bean from adminContact as bean", "admin", "Contact", p);
			List<?> results = bql.beanResults();
			assertNotNull(results);
			assertTrue(results.isEmpty());
		}
		finally {
			p.rollback();
			p.close();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void beanIterableScrollsHqlAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			p.begin();
			HibernateBizQL bql = newPreResolvedBizQL("select bean as bean from adminContact as bean", "admin", "Contact", p);
			try (AutoClosingIterable<?> iter = bql.beanIterable()) {
				assertNotNull(iter);
			}
		}
		finally {
			p.rollback();
			p.close();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void projectedResultsExecutesHqlAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			p.begin();
			HibernateBizQL bql = newPreResolvedBizQL("select bean as bean from adminContact as bean", "admin", "Contact", p);
			List<?> results = bql.projectedResults();
			assertNotNull(results);
			assertTrue(results.isEmpty());
		}
		finally {
			p.rollback();
			p.close();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void projectedIterableScrollsHqlAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			p.begin();
			HibernateBizQL bql = newPreResolvedBizQL("select bean as bean from adminContact as bean", "admin", "Contact", p);
			try (AutoClosingIterable<?> iter = bql.projectedIterable()) {
				assertNotNull(iter);
			}
		}
		finally {
			p.rollback();
			p.close();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void scalarResultsExecutesHqlAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			p.begin();
			HibernateBizQL bql = newPreResolvedBizQL(
					"select bean.bizId as bizId from adminContact as bean", "admin", "Contact", p);
			List<?> results = bql.scalarResults(String.class);
			assertNotNull(results);
			assertTrue(results.isEmpty());
		}
		finally {
			p.rollback();
			p.close();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void scalarIterableScrollsHqlAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			p.begin();
			HibernateBizQL bql = newPreResolvedBizQL(
					"select bean.bizId as bizId from adminContact as bean", "admin", "Contact", p);
			try (AutoClosingIterable<String> iter = bql.scalarIterable(String.class)) {
				assertNotNull(iter);
			}
		}
		finally {
			p.rollback();
			p.close();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void tupleResultsExecutesHqlAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			p.begin();
			HibernateBizQL bql = newPreResolvedBizQL(
					"select bean.bizId as a, bean.bizKey as b from adminContact as bean",
					"admin", "Contact", p);
			List<?> results = bql.tupleResults();
			assertNotNull(results);
			assertTrue(results.isEmpty());
		}
		finally {
			p.rollback();
			p.close();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void tupleIterableScrollsHqlAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			p.begin();
			HibernateBizQL bql = newPreResolvedBizQL(
					"select bean.bizId as a, bean.bizKey as b from adminContact as bean",
					"admin", "Contact", p);
			try (AutoClosingIterable<Object[]> iter = bql.tupleIterable()) {
				assertNotNull(iter);
			}
		}
		finally {
			p.rollback();
			p.close();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void executeRunsUpdateHqlAgainstH2() throws Exception {
		AbstractHibernatePersistenceTest.TestHibernatePersistence p = new AbstractHibernatePersistenceTest.TestHibernatePersistence();
		try {
			p.begin();
			// delete where no rows exist — safe no-op that exercises the execute path
			HibernateBizQL bql = newPreResolvedBizQL(
					"delete from adminContact where bizKey = 'no-such-key'", "admin", "Contact", p);
			int count = bql.execute();
			assertTrue(count >= 0);
		}
		finally {
			p.rollback();
			p.close();
		}
	}

	// -----------------------------------------------------------------------
	// Helpers
	// -----------------------------------------------------------------------

	/**
	 * Construct a {@link HibernateBizQL} whose query string is already resolved.
	 * This bypasses the {@code {module.Document}} → entity-name resolution that
	 * requires full Skyve customer/module metadata context.
	 */
	private static HibernateBizQL newPreResolvedBizQL(String hql,
			String drivingModuleName, String drivingDocumentName,
			AbstractHibernatePersistenceTest.TestHibernatePersistence persistence) throws Exception {
		HibernateBizQL bql = new HibernateBizQL("placeholder", persistence);
		// inject private resolvedQuery in AbstractBizQL
		Field rqField = AbstractBizQL.class.getDeclaredField("resolvedQuery");
		rqField.setAccessible(true);
		rqField.set(bql, hql);
		// inject driving document so HibernateQueryDelegate can build DynamicBeans if needed
		Field mnField = AbstractQuery.class.getDeclaredField("drivingModuleName");
		mnField.setAccessible(true);
		mnField.set(bql, drivingModuleName);
		Field dnField = AbstractQuery.class.getDeclaredField("drivingDocumentName");
		dnField.setAccessible(true);
		dnField.set(bql, drivingDocumentName);
		return bql;
	}
}
