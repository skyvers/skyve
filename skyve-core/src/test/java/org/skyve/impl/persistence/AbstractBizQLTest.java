package org.skyve.impl.persistence;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.NoResultsException;

@SuppressWarnings("static-method")
class AbstractBizQLTest {

	@AfterEach
	@SuppressWarnings({"unchecked", "rawtypes"})
	void clearPersistenceThreadLocal() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal) field.get(null)).remove();
	}

	private static AbstractPersistence mockPersistence(String entityName) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		when(persistence.getDocumentEntityName(anyString(), anyString())).thenReturn(entityName);
		persistence.setForThread();
		return persistence;
	}

	private static AbstractBizQL q() {
		return new AbstractBizQL("select bean from {m.D} as bean");
	}

	@Test
	void noTimeoutSetsMinValue() {
		AbstractBizQL bql = q();
		bql.noTimeout();
		assertEquals(Integer.MIN_VALUE, bql.timeoutInSeconds);
	}

	@Test
	void putParameterStoresValue() {
		AbstractBizQL bql = q();
		bql.putParameter("name", "value");
		assertEquals("value", bql.getParameter("name"));
		assertTrue(bql.getParameterNames().contains("name"));
	}

	@Test
	void putParameterNullValue() {
		AbstractBizQL bql = q();
		bql.putParameter("key", null);
		assertNull(bql.getParameter("key"));
		assertTrue(bql.getParameterNames().contains("key"));
	}

	@Test
	void putParameterReturnsSelf() {
		AbstractBizQL bql = q();
		assertSame(bql, bql.putParameter("x", 1));
	}

	@Test
	void beanResultsReturnsEmpty() {
		assertTrue(q().beanResults().isEmpty());
	}

	@Test
	void beanResultReturnsNullWhenEmpty() {
		assertNull(q().beanResult());
	}

	@Test
	void retrieveBeanThrowsNoResultsExceptionWhenEmpty() {
		assertThrows(NoResultsException.class, () -> q().retrieveBean());
	}

	@Test
	void beanIterableHasNoElements() {
		assertFalse(q().beanIterable().iterator().hasNext());
	}

	@Test
	void projectedResultsReturnsEmpty() {
		assertTrue(q().projectedResults().isEmpty());
	}

	@Test
	void projectedResultReturnsNullWhenEmpty() {
		assertNull(q().projectedResult());
	}

	@Test
	void retrieveProjectedThrowsNoResultsExceptionWhenEmpty() {
		assertThrows(NoResultsException.class, () -> q().retrieveProjected());
	}

	@Test
	void projectedIterableHasNoElements() {
		assertFalse(q().projectedIterable().iterator().hasNext());
	}

	@Test
	void scalarResultsReturnsEmpty() {
		assertTrue(q().scalarResults(String.class).isEmpty());
	}

	@Test
	void scalarResultReturnsNullWhenEmpty() {
		assertNull(q().scalarResult(String.class));
	}

	@Test
	void retrieveScalarThrowsNoResultsExceptionWhenEmpty() {
		assertThrows(NoResultsException.class, () -> q().retrieveScalar(String.class));
	}

	@Test
	void scalarIterableHasNoElements() {
		assertFalse(q().scalarIterable(String.class).iterator().hasNext());
	}

	@Test
	void tupleResultsReturnsEmpty() {
		assertTrue(q().tupleResults().isEmpty());
	}

	@Test
	void tupleResultReturnsNullWhenEmpty() {
		assertNull(q().tupleResult());
	}

	@Test
	void retrieveTupleThrowsNoResultsExceptionWhenEmpty() {
		assertThrows(NoResultsException.class, () -> q().retrieveTuple());
	}

	@Test
	void tupleIterableHasNoElements() {
		assertFalse(q().tupleIterable().iterator().hasNext());
	}

	@Test
	void executeReturnsZero() {
		assertEquals(0, q().execute());
	}

	@Test
	void setFirstResultReturnsSelf() {
		AbstractBizQL bql = q();
		assertSame(bql, bql.setFirstResult(10));
	}

	@Test
	void setMaxResultsReturnsSelf() {
		AbstractBizQL bql = new AbstractBizQL("select bean from {m.D} as bean");
		assertSame(bql, bql.setMaxResults(100));
	}

	@Test
	void toQueryStringResolvesDocumentBraces() {
		mockPersistence("ADM_Contact");
		AbstractBizQL bql = new AbstractBizQL("select bean from {admin.Contact} as bean");
		String resolved = bql.toQueryString();
		assertEquals("select bean from ADM_Contact as bean", resolved);
	}

	@Test
	void toQueryStringCachesResolvedQuery() {
		mockPersistence("ADM_Contact");
		AbstractBizQL bql = new AbstractBizQL("select bean from {admin.Contact} as bean");
		String first = bql.toQueryString();
		String second = bql.toQueryString();
		assertSame(first, second);
	}

	@Test
	void toQueryStringSkipsMalformationCheckWhenFalse() {
		mockPersistence("TestTable");
		AbstractBizQL bql = new AbstractBizQL("select * from plain_table");
		// checkForMalformation=false: no braces is allowed
		String resolved = bql.toQueryString(false);
		assertEquals("select * from plain_table", resolved);
	}

	@Test
	void toQueryStringThrowsWhenNoBracesAndCheckEnabled() {
		mockPersistence("TestTable");
		AbstractBizQL bql = new AbstractBizQL("select * from plain_table");
		// toQueryString(true) → malformation check fails for no braces
		assertThrows(IllegalStateException.class, () -> bql.toQueryString(true));
	}

	@Test
	void resolveDocumentsThrowsWhenMissingClosingBrace() {
		mockPersistence("TestTable");
		AbstractBizQL bql = new AbstractBizQL("select * from {m.Doc");
		// No closing brace → DomainException wrapped in IllegalStateException
		assertThrows(IllegalStateException.class, () -> bql.toQueryString());
	}

	@Test
	void resolveDocumentsThrowsWhenModuleDocumentHasNoDot() {
		mockPersistence("TestTable");
		AbstractBizQL bql = new AbstractBizQL("select * from {NoDocumentName}");
		// No dot in module.document → DomainException wrapped in IllegalStateException
		assertThrows(IllegalStateException.class, () -> bql.toQueryString());
	}

	@Test
	void resolveDocumentsSetsDrivingModuleAndDocument() {
		mockPersistence("ADM_User");
		AbstractBizQL bql = new AbstractBizQL("select bean from {admin.User} as bean");
		bql.toQueryString();
		assertEquals("admin", bql.getDrivingModuleName());
		assertEquals("User", bql.getDrivingDocumentName());
	}

	@Test
	void resolveDocumentsResolvesMultipleBracePairs() {
		mockPersistence("MOD_Doc");
		AbstractBizQL bql = new AbstractBizQL("from {mod.Doc} a, {mod.Doc} b");
		String resolved = bql.toQueryString();
		assertEquals("from MOD_Doc a, MOD_Doc b", resolved);
	}
}
