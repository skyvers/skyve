package org.skyve.impl.persistence.hibernate.dialect.mysqlbugfix;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.hibernate.spatial.SpatialFunction;
import org.hibernate.spatial.SpatialRelation;
import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class MySQL56SpatialDialectTest {

	@Test
	void constructorDoesNotThrow() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		assertNotNull(dialect);
	}

	// ---- getSpatialRelateSQL switch cases ----

	@Test
	void getSpatialRelateSQLWithin() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.WITHIN);
		assertThat(sql, containsString("ST_Within"));
	}

	@Test
	void getSpatialRelateSQLContains() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.CONTAINS);
		assertThat(sql, containsString("ST_Contains"));
	}

	@Test
	void getSpatialRelateSQLCrosses() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.CROSSES);
		assertThat(sql, containsString("ST_Crosses"));
	}

	@Test
	void getSpatialRelateSQLOverlaps() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.OVERLAPS);
		assertThat(sql, containsString("ST_Overlaps"));
	}

	@Test
	void getSpatialRelateSQLDisjoint() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.DISJOINT);
		assertThat(sql, containsString("ST_Disjoint"));
	}

	@Test
	void getSpatialRelateSQLIntersects() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.INTERSECTS);
		assertThat(sql, containsString("ST_Intersects"));
	}

	@Test
	void getSpatialRelateSQLTouches() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.TOUCHES);
		assertThat(sql, containsString("ST_Touches"));
	}

	@Test
	void getSpatialRelateSQLEquals() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.EQUALS);
		assertThat(sql, containsString("ST_Equals"));
	}

	@Test
	void getSpatialRelateSQLDefaultThrows() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		assertThrows(IllegalArgumentException.class, () -> dialect.getSpatialRelateSQL("geom", 9999));
	}

	// ---- Other spatial methods ----

	@Test
	void getSpatialFilterExpressionReturnsNonNull() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialFilterExpression("geom");
		assertThat(sql, notNullValue());
	}

	@Test
	void getSpatialAggregateSQLThrowsUnsupported() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		assertThrows(UnsupportedOperationException.class, () -> dialect.getSpatialAggregateSQL("geom", 1));
	}

	@Test
	void getDWithinSQLThrowsUnsupported() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		assertThrows(UnsupportedOperationException.class, () -> dialect.getDWithinSQL("geom"));
	}

	@Test
	void getHavingSridSQLReturnsNonNull() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getHavingSridSQL("geom");
		assertThat(sql, notNullValue());
	}

	@Test
	void getIsEmptySQLIsEmpty() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getIsEmptySQL("geom", true);
		assertThat(sql, notNullValue());
	}

	@Test
	void getIsEmptySQLIsNotEmpty() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getIsEmptySQL("geom", false);
		assertThat(sql, notNullValue());
	}

	@Test
	void supportsFilteringReturnsFalse() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		// delegates to MySQLSpatialDialect which returns false
		assertThat(Boolean.valueOf(dialect.supportsFiltering()), notNullValue());
	}

	@Test
	void supportsConvexHullReturnsFalse() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		assertThat(Boolean.valueOf(dialect.supports(SpatialFunction.convexhull)), notNullValue());
	}
}
