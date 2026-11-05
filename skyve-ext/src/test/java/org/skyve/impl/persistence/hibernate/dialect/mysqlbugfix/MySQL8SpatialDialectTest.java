package org.skyve.impl.persistence.hibernate.dialect.mysqlbugfix;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.hibernate.spatial.SpatialFunction;
import org.hibernate.spatial.SpatialRelation;
import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class MySQL8SpatialDialectTest {

	@Test
	void constructorDoesNotThrow() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		assertNotNull(dialect);
	}

	// ---- getSpatialRelateSQL switch cases ----

	@Test
	void getSpatialRelateSQLWithin() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.WITHIN);
		assertThat(sql, containsString("ST_within"));
	}

	@Test
	void getSpatialRelateSQLContains() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.CONTAINS);
		assertThat(sql, containsString("ST_contains"));
	}

	@Test
	void getSpatialRelateSQLCrosses() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.CROSSES);
		assertThat(sql, containsString("ST_crosses"));
	}

	@Test
	void getSpatialRelateSQLOverlaps() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.OVERLAPS);
		assertThat(sql, containsString("ST_overlaps"));
	}

	@Test
	void getSpatialRelateSQLDisjoint() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.DISJOINT);
		assertThat(sql, containsString("ST_disjoint"));
	}

	@Test
	void getSpatialRelateSQLIntersects() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.INTERSECTS);
		assertThat(sql, containsString("ST_intersects"));
	}

	@Test
	void getSpatialRelateSQLTouches() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.TOUCHES);
		assertThat(sql, containsString("ST_touches"));
	}

	@Test
	void getSpatialRelateSQLEquals() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.EQUALS);
		assertThat(sql, containsString("ST_equals"));
	}

	@Test
	void getSpatialRelateSQLDefaultThrows() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		assertThrows(IllegalArgumentException.class, () -> dialect.getSpatialRelateSQL("geom", 9999));
	}

	// ---- Other spatial methods ----

	@Test
	void getSpatialFilterExpressionReturnsNonNull() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialFilterExpression("geom");
		assertThat(sql, notNullValue());
	}

	@Test
	void getSpatialAggregateSQLThrowsUnsupported() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		assertThrows(UnsupportedOperationException.class, () -> dialect.getSpatialAggregateSQL("geom", 1));
	}

	@Test
	void getDWithinSQLThrowsUnsupported() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		assertThrows(UnsupportedOperationException.class, () -> dialect.getDWithinSQL("geom"));
	}

	@Test
	void getHavingSridSQLReturnsNonNull() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getHavingSridSQL("geom");
		assertThat(sql, notNullValue());
		assertThat(sql, containsString("ST_SRID"));
	}

	@Test
	void getIsEmptySQLIsEmpty() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getIsEmptySQL("geom", true);
		assertThat(sql, containsString("ST_IsEmpty"));
	}

	@Test
	void getIsEmptySQLIsNotEmpty() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getIsEmptySQL("geom", false);
		assertThat(sql, containsString("NOT"));
	}

	@Test
	void supportsFilteringReturnsTrue() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		assertTrue(dialect.supportsFiltering());
	}

	@Test
	void supportsConvexHullDoesNotThrow() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		// just verify the method can be called without throwing
		dialect.supports(SpatialFunction.convexhull);
		assertNotNull(dialect);
	}

	@Test
	void supportsWithinDoesNotThrow() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		dialect.supports(SpatialFunction.within);
		assertNotNull(dialect);
	}
}
