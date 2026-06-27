package org.skyve.impl.persistence.hibernate.dialect.mysqlbugfix;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;

import org.hibernate.spatial.SpatialFunction;
import org.junit.Test;

@SuppressWarnings("static-method")
public class MySQL5SpatialDialectTest {

	@Test
	public void constructorDoesNotThrow() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		assertNotNull(dialect);
	}

	@Test
	public void getSpatialRelateSQLReturnsSql() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", 1);
		assertThat(sql, notNullValue());
	}

	@Test
	public void getSpatialFilterExpressionReturnsSql() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		String sql = dialect.getSpatialFilterExpression("geom");
		assertThat(sql, notNullValue());
	}

	@Test
	public void getSpatialAggregateSQLThrowsUnsupported() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		assertThrows(UnsupportedOperationException.class, () -> dialect.getSpatialAggregateSQL("geom", 1));
	}

	@Test
	public void getDWithinSQLThrowsUnsupported() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		assertThrows(UnsupportedOperationException.class, () -> dialect.getDWithinSQL("geom"));
	}

	@Test
	public void getHavingSridSQLReturnsSql() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		String sql = dialect.getHavingSridSQL("geom");
		assertThat(sql, notNullValue());
	}

	@Test
	public void getIsEmptySQLReturnsSql() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		String sql = dialect.getIsEmptySQL("geom", true);
		assertThat(sql, notNullValue());
	}

	@Test
	public void supportsFilteringReturnsFalse() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		assertFalse(dialect.supportsFiltering());
	}

	@Test
	public void supportsConvexHull() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		// The return value is delegate-defined; just verify invocation succeeds
		boolean result = dialect.supports(SpatialFunction.convexhull);
		assertFalse(result);
	}
}
