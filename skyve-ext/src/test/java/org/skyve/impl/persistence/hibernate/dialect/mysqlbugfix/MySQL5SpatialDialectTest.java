package org.skyve.impl.persistence.hibernate.dialect.mysqlbugfix;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.hibernate.spatial.SpatialFunction;
import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class MySQL5SpatialDialectTest {

	@Test
	void constructorDoesNotThrow() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		assertNotNull(dialect);
	}

	@Test
	void getSpatialRelateSQLReturnsSql() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", 1);
		assertThat(sql, notNullValue());
	}

	@Test
	void getSpatialFilterExpressionReturnsSql() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		String sql = dialect.getSpatialFilterExpression("geom");
		assertThat(sql, notNullValue());
	}

	@Test
	void getSpatialAggregateSQLThrowsUnsupported() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		assertThrows(UnsupportedOperationException.class, () -> dialect.getSpatialAggregateSQL("geom", 1));
	}

	@Test
	void getDWithinSQLThrowsUnsupported() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		assertThrows(UnsupportedOperationException.class, () -> dialect.getDWithinSQL("geom"));
	}

	@Test
	void getHavingSridSQLReturnsSql() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		String sql = dialect.getHavingSridSQL("geom");
		assertThat(sql, notNullValue());
	}

	@Test
	void getIsEmptySQLReturnsSql() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		String sql = dialect.getIsEmptySQL("geom", true);
		assertThat(sql, notNullValue());
	}

	@Test
	void supportsFilteringReturnsFalse() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		assertFalse(dialect.supportsFiltering());
	}

	@Test
	void supportsConvexHull() {
		MySQL5SpatialDialect dialect = new MySQL5SpatialDialect();
		// The return value is delegate-defined; just verify invocation succeeds
		boolean result = dialect.supports(SpatialFunction.convexhull);
		assertFalse(result);
	}
}
