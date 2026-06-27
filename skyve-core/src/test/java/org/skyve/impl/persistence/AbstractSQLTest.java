package org.skyve.impl.persistence;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.sql.Types;
import java.util.Collections;
import java.util.List;

import org.apache.commons.beanutils.DynaBean;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.mockito.Mockito;
import org.skyve.domain.Bean;
import org.skyve.domain.app.admin.Communication.FormatType;
import org.skyve.domain.messages.NoResultsException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.AutoClosingIterableAdpater;
import org.skyve.persistence.DataStore;
import org.skyve.persistence.SQL;

@SuppressWarnings("resource")
class AbstractSQLTest {

	/**
	 * Empty iterable helper for stub implementations.
	 */
	private static class EmptyAutoClosingIterable<T> implements AutoClosingIterable<T> {
		@Override
		public java.util.Iterator<T> iterator() {
			return java.util.Collections.emptyIterator();
		}

		@Override
		public void close() {
			/* no-op */
		}
	}

	/**
	 * Minimal concrete implementation of AbstractSQL for testing.
	 */
	private static class TestSQL extends AbstractSQL {

		TestSQL(String query) {
			super(query);
		}

		TestSQL(String moduleName, String documentName, String query) {
			super(moduleName, documentName, query);
		}

		TestSQL(Document document, String query) {
			super(document, query);
		}

		@Override
		public <T extends Bean> List<T> beanResults() {
			return Collections.emptyList();
		}


		@Override
		public <T extends Bean> AutoClosingIterable<T> beanIterable() {
			return new EmptyAutoClosingIterable<>();
		}

		@Override
		public List<Object[]> tupleResults() {
			return List.of();
		}


		@Override
		public AutoClosingIterable<Object[]> tupleIterable() {
			return new EmptyAutoClosingIterable<>();
		}

		@Override
		public <T extends Object> List<T> scalarResults(Class<T> type) {
			return Collections.emptyList();
		}


		@Override
		public <T extends Object> AutoClosingIterable<T> scalarIterable(Class<T> type) {
			return new EmptyAutoClosingIterable<>();
		}

		@Override
		public List<DynaBean> dynaResults() {
			return List.of();
		}


		@Override
		public AutoClosingIterable<DynaBean> dynaIterable() {
			return new EmptyAutoClosingIterable<>();
		}

		@Override
		public int execute() {
			return 0;
		}

		@Override
		public String toQueryString() {
			return super.toQueryString();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorWithQueryStringStoresQuery() {
		TestSQL sql = new TestSQL("SELECT 1");
		assertEquals("SELECT 1", sql.toQueryString());
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorWithModuleDocumentQueryStoresModuleAndDocument() {
		TestSQL sql = new TestSQL("myModule", "myDocument", "SELECT 1");
		assertEquals("myModule", sql.getModuleName());
		assertEquals("myDocument", sql.getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterDateOnlyStoresDateType() {
		TestSQL sql = new TestSQL("q");
		DateOnly value = new DateOnly();
		SQL result = sql.putParameter("d", value);
		assertSame(sql, result);
		assertEquals(AttributeType.date, sql.getParameterType("d"));
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterDateTimeStoresDateTimeType() {
		TestSQL sql = new TestSQL("q");
		DateTime value = new DateTime();
		sql.putParameter("dt", value);
		assertEquals(AttributeType.dateTime, sql.getParameterType("dt"));
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterTimeOnlyStoresTimeType() {
		TestSQL sql = new TestSQL("q");
		TimeOnly value = new TimeOnly();
		sql.putParameter("t", value);
		assertEquals(AttributeType.time, sql.getParameterType("t"));
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterTimestampStoresTimestampType() {
		TestSQL sql = new TestSQL("q");
		Timestamp value = new Timestamp();
		sql.putParameter("ts", value);
		assertEquals(AttributeType.timestamp, sql.getParameterType("ts"));
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterDecimalStoresDecimalType() {
		TestSQL sql = new TestSQL("q");
		Decimal10 value = new Decimal10("1.0");
		sql.putParameter("dec", value);
		assertEquals(AttributeType.decimal10, sql.getParameterType("dec"));
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterIntegerStoresIntegerType() {
		TestSQL sql = new TestSQL("q");
		sql.putParameter("i", Integer.valueOf(42));
		assertEquals(AttributeType.integer, sql.getParameterType("i"));
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterLongStoresLongIntegerType() {
		TestSQL sql = new TestSQL("q");
		sql.putParameter("l", Long.valueOf(100L));
		assertEquals(AttributeType.longInteger, sql.getParameterType("l"));
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterStringMemoStoresMemoType() {
		TestSQL sql = new TestSQL("q");
		sql.putParameter("m", "text", true);
		assertEquals(AttributeType.memo, sql.getParameterType("m"));
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterStringTextStoresTextType() {
		TestSQL sql = new TestSQL("q");
		sql.putParameter("s", "text", false);
		assertEquals(AttributeType.text, sql.getParameterType("s"));
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterNullBeanStoresNullWithIdType() {
		TestSQL sql = new TestSQL("q");
		sql.putParameter("bean", (Bean) null);
		assertEquals(AttributeType.id, sql.getParameterType("bean"));
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterBooleanStoresBoolType() {
		TestSQL sql = new TestSQL("q");
		sql.putParameter("b", Boolean.TRUE);
		assertEquals(AttributeType.bool, sql.getParameterType("b"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getParameterTypeReturnsNullForUnknownName() {
		TestSQL sql = new TestSQL("q");
		assertNull(sql.getParameterType("notSet"));
	}

	@Test
	@SuppressWarnings("static-method")
	void noTimeoutSetsNegativeTimeout() {
		TestSQL sql = new TestSQL("q");
		SQL result = sql.noTimeout();
		assertSame(sql, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterGeometryStoresGeometryType() {
		TestSQL sql = new TestSQL("q");
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		sql.putParameter("geo", point);
		assertEquals(AttributeType.geometry, sql.getParameterType("geo"));
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterNullEnumerationStoresNullWithEnumerationType() {
		TestSQL sql = new TestSQL("q");
		sql.putParameter("en", (Enumeration) null);
		assertEquals(AttributeType.enumeration, sql.getParameterType("en"));
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterEnumerationStoresCodeWithEnumerationType() {
		TestSQL sql = new TestSQL("q");
		sql.putParameter("en", FormatType.email);
		assertEquals(AttributeType.enumeration, sql.getParameterType("en"));
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterNonNullBeanStoresBizIdWithIdType() {
		TestSQL sql = new TestSQL("q");
		Bean bean = Mockito.mock(Bean.class);
		Mockito.when(bean.getBizId()).thenReturn("biz-1");
		sql.putParameter("bean", bean);
		assertEquals(AttributeType.id, sql.getParameterType("bean"));
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorWithDocumentStoresModuleAndDocument() {
		Document doc = Mockito.mock(Document.class);
		Mockito.when(doc.getOwningModuleName()).thenReturn("admin");
		Mockito.when(doc.getName()).thenReturn("Contact");
		TestSQL sql = new TestSQL(doc, "SELECT 1");
		assertEquals("admin", sql.getModuleName());
		assertEquals("Contact", sql.getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void beanResultReturnsNullWhenNoResults() {
		TestSQL sql = new TestSQL("q");
		assertNull(sql.beanResult());
	}

	@Test
	@SuppressWarnings("static-method")
	void retrieveBeanThrowsNoResultsExceptionWhenEmpty() {
		TestSQL sql = new TestSQL("q");
		assertThrows(NoResultsException.class, () -> sql.retrieveBean());
	}

	@Test
	@SuppressWarnings("static-method")
	void scalarResultReturnsNullWhenNoResults() {
		TestSQL sql = new TestSQL("q");
		assertNull(sql.scalarResult(String.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void retrieveScalarThrowsNoResultsExceptionWhenEmpty() {
		TestSQL sql = new TestSQL("q");
		assertThrows(NoResultsException.class, () -> sql.retrieveScalar(String.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void tupleResultReturnsNullWhenNoResults() {
		TestSQL sql = new TestSQL("q");
		assertNull(sql.tupleResult());
	}

	@Test
	@SuppressWarnings("static-method")
	void retrieveTupleThrowsNoResultsExceptionWhenEmpty() {
		TestSQL sql = new TestSQL("q");
		assertThrows(NoResultsException.class, () -> sql.retrieveTuple());
	}

	@Test
	@SuppressWarnings("static-method")
	void dynaResultReturnsNullWhenNoResults() {
		TestSQL sql = new TestSQL("q");
		assertNull(sql.dynaResult());
	}

	@Test
	@SuppressWarnings("static-method")
	void retrieveDynaThrowsNoResultsExceptionWhenEmpty() {
		TestSQL sql = new TestSQL("q");
		assertThrows(NoResultsException.class, () -> sql.retrieveDyna());
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsTimeoutWhenPositive() throws Exception { // fix
		TestSQL sql = new TestSQL("SELECT 1");
		sql.setTimeoutInSeconds(30);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setQueryTimeout(30);
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsNullBoolParameter() throws Exception { // fix
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("flag", (Boolean) null);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setNull(eq("flag"), eq(Types.BOOLEAN));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsBoolParameter() throws Exception { // fix
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("flag", Boolean.TRUE);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setBoolean(eq("flag"), eq(true));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsNullVarcharForNullTextParameter() throws Exception { // fix
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("name", (String) null, false);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setNull(eq("name"), eq(Types.VARCHAR));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsStringForTextParameter() throws Exception { // fix
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("name", "Alice", false);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setString(eq("name"), eq("Alice"));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsNullLongvarcharForNullMemoParameter() throws Exception { // fix
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("notes", (String) null, true);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setNull(eq("notes"), eq(Types.LONGVARCHAR));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsStringForMemoParameter() throws Exception { // fix
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("notes", "some notes", true);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setString(eq("notes"), eq("some notes"));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsIntegerParameter() throws Exception { // fix
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("count", Integer.valueOf(42));
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setInt(eq("count"), eq(42));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsLongParameter() throws Exception { // fix
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("bigcount", Long.valueOf(999L));
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setLong(eq("bigcount"), eq(999L));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsBizIdStringForBeanIdParameter() throws Exception { // fix
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		Bean mockBean = mock(Bean.class);
		Mockito.when(mockBean.getBizId()).thenReturn("beanId123");
		sql.putParameter("ref", mockBean);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setString(eq("ref"), eq("beanId123"));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsNullDateParameter() throws Exception {
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("d", (DateOnly) null);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setNull(eq("d"), eq(Types.DATE));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsDateParameter() throws Exception {
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("d", new DateOnly());
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setDate(eq("d"), any(java.sql.Date.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsNullDateTimeParameter() throws Exception {
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("dt", (DateTime) null);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setNull(eq("dt"), eq(Types.TIMESTAMP));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsDateTimeParameter() throws Exception {
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("dt", new DateTime());
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setTimestamp(eq("dt"), any(java.sql.Timestamp.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsNullDecimalParameter() throws Exception {
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("dec", (Decimal10) null);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setNull(eq("dec"), eq(Types.DECIMAL));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsDecimalParameter() throws Exception {
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("dec", new Decimal10(3.14));
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setBigDecimal(eq("dec"), any(java.math.BigDecimal.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsNullTimeParameter() throws Exception {
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("t", (TimeOnly) null);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setNull(eq("t"), eq(Types.TIME));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsTimeParameter() throws Exception {
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("t", new TimeOnly());
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setTime(eq("t"), any(java.sql.Time.class));
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void prepareStatementSetsNullGeometryParameter() throws Exception {
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("geom", (org.locationtech.jts.geom.Geometry) null);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		Mockito.when(dialect.getGeometrySqlType()).thenReturn(Types.OTHER);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setNull(eq("geom"), eq(Types.OTHER));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsNullIntegerParameter() throws Exception {
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("n", (Integer) null);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setNull(eq("n"), eq(Types.INTEGER));
	}

	@Test
	@SuppressWarnings("static-method")
	void prepareStatementSetsNullLongParameter() throws Exception {
		TestSQL sql = new TestSQL("SELECT 1");
		sql.noTimeout();
		sql.putParameter("n", (Long) null);
		NamedParameterPreparedStatement ps = mock(NamedParameterPreparedStatement.class);
		DataStore ds = new DataStore("jndi/test", "org.H2Dialect");
		SkyveDialect dialect = mock(SkyveDialect.class);
		sql.prepareStatement(ps, ds, dialect);
		verify(ps).setNull(eq("n"), eq(Types.NUMERIC));
	}

	@Test
	@SuppressWarnings("static-method")
	void putParameterStringNotMemoStoresTextType() {
		TestSQL sql = new TestSQL("SELECT 1");
		sql.putParameter("t", "short text", false);
		assertEquals(AttributeType.text, sql.getParameterType("t"));
	}

	// ---- result methods returning non-null ----

	private static class StubBean extends org.skyve.impl.domain.AbstractTransientBean {
		private static final long serialVersionUID = 1L;
		@Override public String getBizModule() { return "test"; }
		@Override public String getBizDocument() { return "StubBean"; }
		@Override public String getBizKey() { return "key"; }
	}

	private static class OneResultSQL extends AbstractSQL {
		private final StubBean bean;
		private final org.apache.commons.beanutils.DynaBean dynaBean;

		OneResultSQL(String query, StubBean bean) {
			super(query);
			this.bean = bean;
			this.dynaBean = org.mockito.Mockito.mock(org.apache.commons.beanutils.DynaBean.class);
		}

		@SuppressWarnings("unchecked")
		@Override
		public <T extends org.skyve.domain.Bean> List<T> beanResults() {
			return java.util.Collections.singletonList((T) bean);
		}

		@SuppressWarnings("unchecked")
		@Override
		public <T extends org.skyve.domain.Bean> AutoClosingIterable<T> beanIterable() {
			return new AutoClosingIterableAdpater<>(java.util.Collections.singletonList((T) bean));
		}

		@Override
		public List<Object[]> tupleResults() {
			return java.util.Collections.singletonList(new Object[]{bean});
		}

		@Override
		public AutoClosingIterable<Object[]> tupleIterable() {
			return new AutoClosingIterableAdpater<>(java.util.Collections.singletonList(new Object[]{bean}));
		}

		@SuppressWarnings("unchecked")
		@Override
		public <T> List<T> scalarResults(Class<T> type) {
			return java.util.Collections.singletonList((T) bean);
		}

		@SuppressWarnings("unchecked")
		@Override
		public <T> AutoClosingIterable<T> scalarIterable(Class<T> type) {
			return new AutoClosingIterableAdpater<>(java.util.Collections.singletonList((T) bean));
		}

		@Override
		public List<org.apache.commons.beanutils.DynaBean> dynaResults() {
			return java.util.Collections.singletonList(dynaBean);
		}

		@Override
		public AutoClosingIterable<org.apache.commons.beanutils.DynaBean> dynaIterable() {
			return new AutoClosingIterableAdpater<>(java.util.Collections.singletonList(dynaBean));
		}

		@Override
		public int execute() { return 1; }

		@Override
		public String toQueryString() { return super.toQueryString(); }
	}

	@Test
	@SuppressWarnings("static-method")
	void beanResultReturnsFirstBeanWhenListHasOneItem() {
		StubBean bean = new StubBean();
		OneResultSQL sql = new OneResultSQL("SELECT 1", bean);
		assertSame(bean, sql.beanResult());
	}

	@Test
	@SuppressWarnings("static-method")
	void retrieveBeanReturnsBeanWhenListHasOneItem() {
		StubBean bean = new StubBean();
		OneResultSQL sql = new OneResultSQL("SELECT 1", bean);
		assertSame(bean, sql.retrieveBean());
	}

	@Test
	@SuppressWarnings("static-method")
	void scalarResultReturnsFirstItemWhenListHasOneItem() {
		StubBean bean = new StubBean();
		OneResultSQL sql = new OneResultSQL("SELECT 1", bean);
		assertSame(bean, sql.scalarResult(Object.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void retrieveScalarReturnsItemWhenListHasOneItem() {
		StubBean bean = new StubBean();
		OneResultSQL sql = new OneResultSQL("SELECT 1", bean);
		assertSame(bean, sql.retrieveScalar(Object.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void tupleResultReturnsFirstTupleWhenListHasOneItem() {
		OneResultSQL sql = new OneResultSQL("SELECT 1", new StubBean());
		assertNotNull(sql.tupleResult());
	}

	@Test
	@SuppressWarnings("static-method")
	void retrieveTupleReturnsTupleWhenListHasOneItem() {
		OneResultSQL sql = new OneResultSQL("SELECT 1", new StubBean());
		assertNotNull(sql.retrieveTuple());
	}

	@Test
	@SuppressWarnings("static-method")
	void dynaResultReturnsDynaBeanWhenListHasOneItem() {
		OneResultSQL sql = new OneResultSQL("SELECT 1", new StubBean());
		assertNotNull(sql.dynaResult());
	}

	@Test
	@SuppressWarnings("static-method")
	void retrieveDynaReturnsDynaBeanWhenListHasOneItem() {
		OneResultSQL sql = new OneResultSQL("SELECT 1", new StubBean());
		assertNotNull(sql.retrieveDyna());
	}
}
