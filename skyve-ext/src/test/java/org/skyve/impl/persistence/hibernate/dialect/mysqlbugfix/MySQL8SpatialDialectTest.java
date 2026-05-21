package org.skyve.impl.persistence.hibernate.dialect.mysqlbugfix;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import org.hibernate.spatial.SpatialFunction;
import org.hibernate.spatial.SpatialRelation;
import org.junit.Test;

@SuppressWarnings("static-method")
public class MySQL8SpatialDialectTest {

	@Test
	public void constructorDoesNotThrow() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		assertNotNull(dialect);
	}

	// ---- getSpatialRelateSQL switch cases ----

	@Test
	public void getSpatialRelateSQLWithin() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.WITHIN);
		assertThat(sql, containsString("ST_within"));
	}

	@Test
	public void getSpatialRelateSQLContains() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.CONTAINS);
		assertThat(sql, containsString("ST_contains"));
	}

	@Test
	public void getSpatialRelateSQLCrosses() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.CROSSES);
		assertThat(sql, containsString("ST_crosses"));
	}

	@Test
	public void getSpatialRelateSQLOverlaps() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.OVERLAPS);
		assertThat(sql, containsString("ST_overlaps"));
	}

	@Test
	public void getSpatialRelateSQLDisjoint() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.DISJOINT);
		assertThat(sql, containsString("ST_disjoint"));
	}

	@Test
	public void getSpatialRelateSQLIntersects() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.INTERSECTS);
		assertThat(sql, containsString("ST_intersects"));
	}

	@Test
	public void getSpatialRelateSQLTouches() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.TOUCHES);
		assertThat(sql, containsString("ST_touches"));
	}

	@Test
	public void getSpatialRelateSQLEquals() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.EQUALS);
		assertThat(sql, containsString("ST_equals"));
	}

	@Test
	public void getSpatialRelateSQLDefaultThrows() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		assertThrows(IllegalArgumentException.class, () -> dialect.getSpatialRelateSQL("geom", 9999));
	}

	// ---- Other spatial methods ----

	@Test
	public void getSpatialFilterExpressionReturnsNonNull() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getSpatialFilterExpression("geom");
		assertThat(sql, notNullValue());
	}

	@Test
	public void getSpatialAggregateSQLThrowsUnsupported() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		assertThrows(UnsupportedOperationException.class, () -> dialect.getSpatialAggregateSQL("geom", 1));
	}

	@Test
	public void getDWithinSQLThrowsUnsupported() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		assertThrows(UnsupportedOperationException.class, () -> dialect.getDWithinSQL("geom"));
	}

	@Test
	public void getHavingSridSQLReturnsNonNull() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getHavingSridSQL("geom");
		assertThat(sql, notNullValue());
		assertThat(sql, containsString("ST_SRID"));
	}

	@Test
	public void getIsEmptySQLIsEmpty() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getIsEmptySQL("geom", true);
		assertThat(sql, containsString("ST_IsEmpty"));
	}

	@Test
	public void getIsEmptySQLIsNotEmpty() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		String sql = dialect.getIsEmptySQL("geom", false);
		assertThat(sql, containsString("NOT"));
	}

	@Test
	public void supportsFilteringReturnsTrue() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		assertTrue(dialect.supportsFiltering());
	}

	@Test
	public void supportsConvexHullDoesNotThrow() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		// just verify the method can be called without throwing
		dialect.supports(SpatialFunction.convexhull);
	}

	@Test
	public void supportsWithinDoesNotThrow() {
		MySQL8SpatialDialect dialect = new MySQL8SpatialDialect();
		dialect.supports(SpatialFunction.within);
	}
}
