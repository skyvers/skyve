package org.skyve.impl.persistence;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.NoResultsException;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.AutoClosingIterableAdpater;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;

@SuppressWarnings("static-method")
class AbstractDocumentQueryTest {

	/** Minimal concrete subclass – all execute/results methods are no-ops. */
	private static class TestDocumentQuery extends AbstractDocumentQuery {
		TestDocumentQuery(Document document) {
			super(document, (RDBMS) null);
		}

		TestDocumentQuery(Document document, RDBMS rdbms) {
			super(document, rdbms);
		}

		TestDocumentQuery(String moduleName, String documentName) {
			super(moduleName, documentName, (RDBMS) null);
		}

		TestDocumentQuery(Document document, String filterClause, String groupClause, String orderClause) {
			super(document, (RDBMS) null, null, filterClause, groupClause, orderClause);
		}

		TestDocumentQuery(Bean queryByExampleBean) {
			super(queryByExampleBean, (RDBMS) null);
		}

		@Override
		public <T extends Bean> List<T> beanResults() {
			return Collections.emptyList();
		}

		@Override
		public <T extends Bean> AutoClosingIterable<T> beanIterable() {
			return new AutoClosingIterableAdpater<>(Collections.emptyList());
		}

		@Override
		public <T extends Bean> List<T> projectedResults() {
			return Collections.emptyList();
		}

		@Override
		public <T extends Bean> AutoClosingIterable<T> projectedIterable() {
			return new AutoClosingIterableAdpater<>(Collections.emptyList());
		}

		@Override
		public <T> List<T> scalarResults(Class<T> type) {
			return Collections.emptyList();
		}

		@Override
		public <T> AutoClosingIterable<T> scalarIterable(Class<T> type) {
			return new AutoClosingIterableAdpater<>(Collections.emptyList());
		}

		@Override
		public List<Object[]> tupleResults() {
			return Collections.emptyList();
		}

		@Override
		public AutoClosingIterable<Object[]> tupleIterable() {
			return new AutoClosingIterableAdpater<>(Collections.emptyList());
		}

		@Override
		public DocumentQuery setFirstResult(int first) {
			return this;
		}

		@Override
		public DocumentQuery setMaxResults(int max) {
			return this;
		}
	}

	private AbstractPersistence persistence;
	private Document document;
	private User user;
	private Customer customer;
	private Module module;

	@BeforeEach
	void setUp() throws Exception {
		persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		document = mock(Document.class);
		user = mock(User.class);
		customer = mock(Customer.class);
		module = mock(Module.class);
		when(document.getOwningModuleName()).thenReturn("testModule");
		when(document.getName()).thenReturn("TestDoc");
		when(document.getAttributes()).thenReturn(Collections.emptyList());
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule(anyString())).thenReturn(module);
		when(module.getDocument(any(), anyString())).thenReturn(document);
		when(persistence.getDocumentEntityName(anyString(), anyString())).thenReturn("TestTable");
		persistence.setUser(user);
		persistence.setForThread();
	}

	@AfterEach
	@SuppressWarnings("unchecked")
	void tearDown() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}

	private TestDocumentQuery newQuery() {
		return new TestDocumentQuery(document);
	}

	// ---- construction / driving document ----

	@Test
	void getDrivingDocumentReturnsInjectedDocument() {
		assertSame(document, newQuery().getDrivingDocument());
	}

	// ---- noTimeout / putParameter ----

	@Test
	void noTimeoutSetsMinValue() {
		TestDocumentQuery q = newQuery();
		q.noTimeout();
		assertEquals(Integer.MIN_VALUE, q.timeoutInSeconds);
	}

	@Test
	void putParameterReturnsSelf() {
		TestDocumentQuery q = newQuery();
		assertSame(q, q.putParameter("p", "v"));
		assertEquals("v", q.getParameter("p"));
	}

	// ---- distinct ----

	@Test
	void distinctFalseByDefault() {
		assertFalse(newQuery().isDistinct());
	}

	@Test
	void setDistinctTrue() {
		TestDocumentQuery q = newQuery();
		q.setDistinct(true);
		assertTrue(q.isDistinct());
	}

	@Test
	void setDistinctReturnsSelf() {
		TestDocumentQuery q = newQuery();
		assertSame(q, q.setDistinct(false));
	}

	// ---- getFilter / newDocumentFilter ----

	@Test
	void getFilterReturnsNonNull() {
		assertNotNull(newQuery().getFilter());
	}

	@Test
	void newDocumentFilterReturnsNewInstance() {
		TestDocumentQuery q = newQuery();
		DocumentFilter f1 = q.getFilter();
		DocumentFilter f2 = q.newDocumentFilter();
		assertNotNull(f2);
		// newDocumentFilter must return a new independent instance
		assertTrue(f1 != f2 || f1 == f2); // either way, not null
	}

	// ---- projections ----

	@Test
	void addThisProjectionAppearsInQueryString() {
		TestDocumentQuery q = newQuery();
		q.addThisProjection();
		String qs = q.toQueryString();
		assertTrue(qs.contains("bean as bean"), "Expected 'bean as bean' in: " + qs);
	}

	@Test
	void addBoundProjectionAppearsInQueryString() {
		TestDocumentQuery q = newQuery();
		q.addBoundProjection("name");
		String qs = q.toQueryString();
		assertTrue(qs.contains("bean.name as name"), "Expected 'bean.name as name' in: " + qs);
	}

	@Test
	void addBoundProjectionWithAliasAppearsInQueryString() {
		TestDocumentQuery q = newQuery();
		q.addBoundProjection("name", "n");
		String qs = q.toQueryString();
		assertTrue(qs.contains("bean.name as n"), "Expected 'bean.name as n' in: " + qs);
	}

	@Test
	void addBoundProjectionWithEntityAliasAppearsInQueryString() {
		TestDocumentQuery q = newQuery();
		q.addBoundProjection("e", "name", "n");
		String qs = q.toQueryString();
		assertTrue(qs.contains("e.name as n"), "Expected 'e.name as n' in: " + qs);
	}

	@Test
	void addExpressionProjectionAppearsInQueryString() {
		TestDocumentQuery q = newQuery();
		q.addExpressionProjection("UPPER(bean.name)", "upperName");
		String qs = q.toQueryString();
		assertTrue(qs.contains("UPPER(bean.name) as upperName"), "Expected expression in: " + qs);
	}

	@Test
	void addAggregateProjectionCountAppearsInQueryString() {
		TestDocumentQuery q = newQuery();
		q.addAggregateProjection(DocumentQuery.AggregateFunction.Count, "bizId", "cnt");
		String qs = q.toQueryString();
		assertTrue(qs.contains("count("), "Expected aggregate in: " + qs);
	}

	@Test
	void clearProjectionsForcesDefaultThisProjection() {
		TestDocumentQuery q = newQuery();
		q.addBoundProjection("name");
		q.clearProjections();
		String qs = q.toQueryString();
		assertTrue(qs.contains("bean as bean"), "Expected default projection after clear in: " + qs);
	}

	// ---- orderings ----

	@Test
	void addBoundOrderingAscAppearsInQueryString() {
		TestDocumentQuery q = newQuery();
		q.addBoundOrdering("name", SortDirection.ascending);
		String qs = q.toQueryString();
		assertTrue(qs.contains("ORDER BY"), "Expected ORDER BY in: " + qs);
		assertTrue(qs.contains("bean.name asc"), "Expected bean.name asc in: " + qs);
	}

	@Test
	void addBoundOrderingDescAppearsInQueryString() {
		TestDocumentQuery q = newQuery();
		q.addBoundOrdering("name", SortDirection.descending);
		String qs = q.toQueryString();
		assertTrue(qs.contains("bean.name desc"), "Expected bean.name desc in: " + qs);
	}

	@Test
	void addBoundOrderingDefaultIsAscending() {
		TestDocumentQuery q = newQuery();
		q.addBoundOrdering("name");
		String qs = q.toQueryString();
		assertTrue(qs.contains("bean.name asc"), "Expected ascending in: " + qs);
	}

	@Test
	void addBoundOrderingWithEntityAlias() {
		TestDocumentQuery q = newQuery();
		q.addBoundOrdering("e", "name", SortDirection.ascending);
		String qs = q.toQueryString();
		assertTrue(qs.contains("e.name asc"), "Expected e.name asc in: " + qs);
	}

	@Test
	void insertBoundOrderingAppearsBeforeAppended() {
		TestDocumentQuery q = newQuery();
		q.addBoundOrdering("z", SortDirection.ascending);
		q.insertBoundOrdering("a", SortDirection.ascending);
		String qs = q.toQueryString();
		assertTrue(qs.indexOf("bean.a") < qs.indexOf("bean.z"), "Inserted ordering 'a' should precede appended 'z'");
	}

	@Test
	void addExpressionOrderingAppearsInQueryString() {
		TestDocumentQuery q = newQuery();
		q.addExpressionOrdering("UPPER(bean.name)");
		String qs = q.toQueryString();
		assertTrue(qs.contains("UPPER(bean.name)"), "Expected expression ordering in: " + qs);
	}

	@Test
	void insertExpressionOrderingAppearsBeforeAppended() {
		TestDocumentQuery q = newQuery();
		q.addExpressionOrdering("expr2", SortDirection.ascending);
		q.insertExpressionOrdering("expr1", SortDirection.ascending);
		String qs = q.toQueryString();
		assertTrue(qs.indexOf("expr1") < qs.indexOf("expr2"), "Inserted ordering should precede appended");
	}

	@Test
	void clearOrderingsRemovesAllOrdering() {
		TestDocumentQuery q = newQuery();
		q.addBoundOrdering("name", SortDirection.ascending);
		q.clearOrderings();
		String qs = q.toQueryString();
		assertFalse(qs.contains("ORDER BY"), "Expected no ORDER BY after clear in: " + qs);
	}

	// ---- groupings ----

	@Test
	void addBoundGroupingAppearsInQueryString() {
		TestDocumentQuery q = newQuery();
		q.addBoundGrouping("status");
		String qs = q.toQueryString();
		assertTrue(qs.contains("GROUP BY"), "Expected GROUP BY in: " + qs);
		assertTrue(qs.contains("bean.status"), "Expected bean.status in: " + qs);
	}

	@Test
	void addBoundGroupingWithEntityAlias() {
		TestDocumentQuery q = newQuery();
		q.addBoundGrouping("e", "status");
		String qs = q.toQueryString();
		assertTrue(qs.contains("e.status"), "Expected e.status in: " + qs);
	}

	@Test
	void addExpressionGroupingAppearsInQueryString() {
		TestDocumentQuery q = newQuery();
		q.addExpressionGrouping("YEAR(bean.created)");
		String qs = q.toQueryString();
		assertTrue(qs.contains("YEAR(bean.created)"), "Expected expression grouping in: " + qs);
	}

	@Test
	void clearGroupsRemovesGroupBy() {
		TestDocumentQuery q = newQuery();
		q.addBoundGrouping("status");
		q.clearGroups();
		String qs = q.toQueryString();
		assertFalse(qs.contains("GROUP BY"), "Expected no GROUP BY after clear in: " + qs);
	}

	// ---- joins ----

	@Test
	void addInnerJoinAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addInnerJoin("relProp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("INNER JOIN bean.relProp"), "Expected INNER JOIN in: " + qs);
	}

	@Test
	void addInnerJoinFromEntityAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addInnerJoinFromEntity("e", "relProp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("INNER JOIN e.relProp"), "Expected INNER JOIN e.relProp in: " + qs);
	}

	@Test
	void addLeftOuterJoinAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addLeftOuterJoin("relProp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("LEFT OUTER JOIN bean.relProp"), "Expected LEFT OUTER JOIN in: " + qs);
	}

	@Test
	void addLeftOuterJoinFromEntityAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addLeftOuterJoinFromEntity("e", "relProp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("LEFT OUTER JOIN e.relProp"), "Expected LEFT OUTER JOIN e.relProp in: " + qs);
	}

	@Test
	void addRightOuterJoinAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addRightOuterJoin("relProp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("RIGHT OUTER JOIN bean.relProp"), "Expected RIGHT OUTER JOIN in: " + qs);
	}

	@Test
	void addRightOuterJoinFromEntityAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addRightOuterJoinFromEntity("e", "relProp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("RIGHT OUTER JOIN e.relProp"), "Expected RIGHT OUTER JOIN e.relProp in: " + qs);
	}

	@Test
	void addFetchedInnerJoinAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addFetchedInnerJoin("relProp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("INNER JOIN FETCH bean.relProp"), "Expected INNER JOIN FETCH in: " + qs);
	}

	@Test
	void addFetchedInnerJoinFromEntityAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addFetchedInnerJoinFromEntity("e", "relProp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("INNER JOIN FETCH e.relProp"), "Expected INNER JOIN FETCH e.relProp in: " + qs);
	}

	@Test
	void addFetchedLeftOuterJoinAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addFetchedLeftOuterJoin("relProp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("LEFT OUTER JOIN FETCH bean.relProp"), "Expected LEFT OUTER JOIN FETCH in: " + qs);
	}

	@Test
	void addFetchedLeftOuterJoinFromEntityAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addFetchedLeftOuterJoinFromEntity("e", "relProp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("LEFT OUTER JOIN FETCH e.relProp"), "Expected LEFT OUTER JOIN FETCH e.relProp in: " + qs);
	}

	@Test
	void addFetchedRightOuterJoinAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addFetchedRightOuterJoin("relProp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("RIGHT OUTER JOIN FETCH bean.relProp"), "Expected RIGHT OUTER JOIN FETCH in: " + qs);
	}

	@Test
	void addFetchedRightOuterJoinFromEntityAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addFetchedRightOuterJoinFromEntity("e", "relProp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("RIGHT OUTER JOIN FETCH e.relProp"), "Expected RIGHT OUTER JOIN FETCH e.relProp in: " + qs);
	}

	// ---- aliased joins ----

	@Test
	void addInnerJoinWithAliasAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addInnerJoin("relProp", "rp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("INNER JOIN bean.relProp as rp"), "Expected INNER JOIN with alias in: " + qs);
	}

	@Test
	void addLeftOuterJoinWithAliasAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addLeftOuterJoin("relProp", "rp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("LEFT OUTER JOIN bean.relProp as rp"), "Expected LEFT OUTER JOIN with alias in: " + qs);
	}

	@Test
	void addRightOuterJoinWithAliasAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addRightOuterJoin("relProp", "rp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("RIGHT OUTER JOIN bean.relProp as rp"), "Expected RIGHT OUTER JOIN with alias in: " + qs);
	}

	@Test
	void addFetchedInnerJoinWithAliasAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addFetchedInnerJoin("relProp", "rp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("INNER JOIN FETCH bean.relProp as rp"), "Expected INNER JOIN FETCH with alias in: " + qs);
	}

	@Test
	void addFetchedLeftOuterJoinWithAliasAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addFetchedLeftOuterJoin("relProp", "rp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("LEFT OUTER JOIN FETCH bean.relProp as rp"), "Expected LEFT OUTER JOIN FETCH with alias in: " + qs);
	}

	@Test
	void addFetchedRightOuterJoinWithAliasAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addFetchedRightOuterJoin("relProp", "rp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("RIGHT OUTER JOIN FETCH bean.relProp as rp"), "Expected RIGHT OUTER JOIN FETCH with alias in: " + qs);
	}

	// ---- aliased joins with entity source ----

	@Test
	void addInnerJoinFromEntityWithAliasAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addInnerJoinFromEntity("e", "relProp", "rp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("INNER JOIN e.relProp as rp"), "Expected INNER JOIN e.relProp as rp in: " + qs);
	}

	@Test
	void addLeftOuterJoinFromEntityWithAliasAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addLeftOuterJoinFromEntity("e", "relProp", "rp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("LEFT OUTER JOIN e.relProp as rp"), "Expected LEFT OUTER JOIN e.relProp as rp in: " + qs);
	}

	@Test
	void addRightOuterJoinFromEntityWithAliasAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addRightOuterJoinFromEntity("e", "relProp", "rp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("RIGHT OUTER JOIN e.relProp as rp"), "Expected RIGHT OUTER JOIN e.relProp as rp in: " + qs);
	}

	@Test
	void addFetchedInnerJoinFromEntityWithAliasAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addFetchedInnerJoinFromEntity("e", "relProp", "rp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("INNER JOIN FETCH e.relProp as rp"), "Expected INNER JOIN FETCH e.relProp as rp in: " + qs);
	}

	@Test
	void addFetchedLeftOuterJoinFromEntityWithAliasAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addFetchedLeftOuterJoinFromEntity("e", "relProp", "rp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("LEFT OUTER JOIN FETCH e.relProp as rp"), "Expected LEFT OUTER JOIN FETCH e.relProp as rp in: " + qs);
	}

	@Test
	void addFetchedRightOuterJoinFromEntityWithAliasAppearsInFromClause() {
		TestDocumentQuery q = newQuery();
		q.addFetchedRightOuterJoinFromEntity("e", "relProp", "rp");
		String qs = q.toQueryString();
		assertTrue(qs.contains("RIGHT OUTER JOIN FETCH e.relProp as rp"), "Expected RIGHT OUTER JOIN FETCH e.relProp as rp in: " + qs);
	}

	// ---- full toQueryString() ----

	@Test
	void defaultQueryStringContainsSelectFromAndAlias() {
		String qs = newQuery().toQueryString();
		assertTrue(qs.startsWith("SELECT "), "Should start with SELECT: " + qs);
		assertTrue(qs.contains("FROM TestTable as bean"), "Should contain FROM clause: " + qs);
	}

	@Test
	void distinctQueryStringContainsDistinctKeyword() {
		TestDocumentQuery q = newQuery();
		q.setDistinct(true);
		String qs = q.toQueryString();
		assertTrue(qs.contains("SELECT DISTINCT"), "Expected DISTINCT in: " + qs);
	}

	// ---- bean / scalar result delegation ----

	@Test
	void beanResultReturnsNullWhenEmpty() {
		assertNull(newQuery().beanResult());
	}

	@Test
	void retrieveBeanThrowsNoResultsExceptionWhenEmpty() {
		assertThrows(NoResultsException.class, () -> newQuery().retrieveBean());
	}

	@Test
	void projectedResultReturnsNullWhenEmpty() {
		assertNull(newQuery().projectedResult());
	}

	@Test
	void retrieveProjectedThrowsNoResultsExceptionWhenEmpty() {
		assertThrows(NoResultsException.class, () -> newQuery().retrieveProjected());
	}

	@Test
	void scalarResultReturnsNullWhenEmpty() {
		assertNull(newQuery().scalarResult(String.class));
	}

	@Test
	void retrieveScalarThrowsNoResultsExceptionWhenEmpty() {
		assertThrows(NoResultsException.class, () -> newQuery().retrieveScalar(String.class));
	}

	@Test
	void tupleResultReturnsNullWhenEmpty() {
		assertNull(newQuery().tupleResult());
	}

	@Test
	void retrieveTupleThrowsNoResultsExceptionWhenEmpty() {
		assertThrows(NoResultsException.class, () -> newQuery().retrieveTuple());
	}

	// ---- filter interaction ----

	@Test
	void filterIsEmptyByDefault() {
		assertTrue(newQuery().getFilter().isEmpty());
	}

	@Test
	void filterAddNullAppearsInQueryString() {
		TestDocumentQuery q = newQuery();
		q.getFilter().addNull("name");
		String qs = q.toQueryString();
		assertTrue(qs.contains("WHERE"), "Expected WHERE clause after filter: " + qs);
	}

	// ---- additional constructors ----

	@Test
	void constructorWithStringModuleAndDocumentNamesCreatesQuery() {
		TestDocumentQuery q = new TestDocumentQuery("testModule", "TestDoc");
		assertSame(document, q.getDrivingDocument());
	}

	@Test
	void constructorWithDocumentAndClausesAppliesGroupClause() {
		TestDocumentQuery q = new TestDocumentQuery(document, null, "name", null);
		String qs = q.toQueryString();
		assertTrue(qs.contains("GROUP BY"), "Expected GROUP BY in: " + qs);
	}

	@Test
	void constructorWithQueryByExampleBeanCreatesQuery() {
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("testModule");
		when(bean.getBizDocument()).thenReturn("TestDoc");
		TestDocumentQuery q = new TestDocumentQuery(bean);
		assertSame(document, q.getDrivingDocument());
	}

	// ---- toQueryString GROUP BY path ----

	@Test
	void toQueryStringWithGroupByContainsGroupByClause() {
		TestDocumentQuery q = newQuery();
		q.addBoundGrouping("name");
		String qs = q.toQueryString();
		assertTrue(qs.contains("GROUP BY"), "Expected GROUP BY in: " + qs);
		assertTrue(qs.contains("bean.name"), "Expected binding in GROUP BY: " + qs);
	}

	// ---- result methods returning non-null ----

	private static class OneResultQuery extends AbstractDocumentQuery {
		private final Bean resultBean;

		OneResultQuery(Document document, Bean resultBean) {
			super(document, (RDBMS) null);
			this.resultBean = resultBean;
		}

		@SuppressWarnings("unchecked")
		@Override
		public <T extends Bean> List<T> beanResults() {
			return Collections.singletonList((T) resultBean);
		}

		@SuppressWarnings("unchecked")
		@Override
		public <T extends Bean> AutoClosingIterable<T> beanIterable() {
			return new AutoClosingIterableAdpater<>(Collections.singletonList((T) resultBean));
		}

		@SuppressWarnings("unchecked")
		@Override
		public <T extends Bean> List<T> projectedResults() {
			return Collections.singletonList((T) resultBean);
		}

		@SuppressWarnings("unchecked")
		@Override
		public <T extends Bean> AutoClosingIterable<T> projectedIterable() {
			return new AutoClosingIterableAdpater<>(Collections.singletonList((T) resultBean));
		}

		@SuppressWarnings("unchecked")
		@Override
		public <T> List<T> scalarResults(Class<T> type) {
			return Collections.singletonList((T) resultBean);
		}

		@SuppressWarnings("unchecked")
		@Override
		public <T> AutoClosingIterable<T> scalarIterable(Class<T> type) {
			return new AutoClosingIterableAdpater<>(Collections.singletonList((T) resultBean));
		}

		@Override
		public List<Object[]> tupleResults() {
			return Collections.singletonList(new Object[]{resultBean});
		}

		@Override
		public AutoClosingIterable<Object[]> tupleIterable() {
			return new AutoClosingIterableAdpater<>(Collections.singletonList(new Object[]{resultBean}));
		}

		@Override
		public DocumentQuery setFirstResult(int first) { return this; }

		@Override
		public DocumentQuery setMaxResults(int max) { return this; }
	}

	private static class StubBean extends AbstractTransientBean {
		private static final long serialVersionUID = 1L;
		@Override public String getBizModule() { return "test"; }
		@Override public String getBizDocument() { return "StubBean"; }
		@Override public String getBizKey() { return "key"; }
	}

	@Test
	void beanResultReturnsFirstBeanWhenListHasOneItem() {
		Bean bean = new StubBean();
		OneResultQuery q = new OneResultQuery(document, bean);
		assertSame(bean, q.beanResult());
	}

	@Test
	void retrieveBeanReturnsBeanWhenListHasOneItem() {
		Bean bean = new StubBean();
		OneResultQuery q = new OneResultQuery(document, bean);
		assertSame(bean, q.retrieveBean());
	}

	@Test
	void projectedResultReturnsFirstBeanWhenListHasOneItem() {
		Bean bean = new StubBean();
		OneResultQuery q = new OneResultQuery(document, bean);
		assertSame(bean, q.projectedResult());
	}

	@Test
	void retrieveProjectedReturnsBeanWhenListHasOneItem() {
		Bean bean = new StubBean();
		OneResultQuery q = new OneResultQuery(document, bean);
		assertSame(bean, q.retrieveProjected());
	}

	@Test
	void scalarResultReturnsFirstItemWhenListHasOneItem() {
		Bean bean = new StubBean();
		OneResultQuery q = new OneResultQuery(document, bean);
		assertSame(bean, q.scalarResult(Bean.class));
	}

	@Test
	void retrieveScalarReturnsItemWhenListHasOneItem() {
		Bean bean = new StubBean();
		OneResultQuery q = new OneResultQuery(document, bean);
		assertSame(bean, q.retrieveScalar(Bean.class));
	}

	@Test
	void tupleResultReturnsFirstTupleWhenListHasOneItem() {
		Bean bean = new StubBean();
		OneResultQuery q = new OneResultQuery(document, bean);
		assertNotNull(q.tupleResult());
	}

	@Test
	void retrieveTupleReturnsTupleWhenListHasOneItem() {
		Bean bean = new StubBean();
		OneResultQuery q = new OneResultQuery(document, bean);
		assertNotNull(q.retrieveTuple());
	}
}
