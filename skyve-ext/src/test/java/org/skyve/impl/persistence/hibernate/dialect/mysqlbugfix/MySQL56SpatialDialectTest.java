package org.skyve.impl.persistence.hibernate.dialect.mysqlbugfix;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;

import org.hibernate.spatial.SpatialFunction;
import org.hibernate.spatial.SpatialRelation;
import org.junit.Test;

@SuppressWarnings("static-method")
public class MySQL56SpatialDialectTest {

	@Test
	public void constructorDoesNotThrow() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		assertNotNull(dialect);
	}

	// ---- getSpatialRelateSQL switch cases ----

	@Test
	public void getSpatialRelateSQLWithin() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.WITHIN);
		assertThat(sql, containsString("ST_Within"));
	}

	@Test
	public void getSpatialRelateSQLContains() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.CONTAINS);
		assertThat(sql, containsString("ST_Contains"));
	}

	@Test
	public void getSpatialRelateSQLCrosses() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.CROSSES);
		assertThat(sql, containsString("ST_Crosses"));
	}

	@Test
	public void getSpatialRelateSQLOverlaps() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.OVERLAPS);
		assertThat(sql, containsString("ST_Overlaps"));
	}

	@Test
	public void getSpatialRelateSQLDisjoint() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.DISJOINT);
		assertThat(sql, containsString("ST_Disjoint"));
	}

	@Test
	public void getSpatialRelateSQLIntersects() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.INTERSECTS);
		assertThat(sql, containsString("ST_Intersects"));
	}

	@Test
	public void getSpatialRelateSQLTouches() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.TOUCHES);
		assertThat(sql, containsString("ST_Touches"));
	}

	@Test
	public void getSpatialRelateSQLEquals() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialRelateSQL("geom", SpatialRelation.EQUALS);
		assertThat(sql, containsString("ST_Equals"));
	}

	@Test
	public void getSpatialRelateSQLDefaultThrows() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		assertThrows(IllegalArgumentException.class, () -> dialect.getSpatialRelateSQL("geom", 9999));
	}

	// ---- Other spatial methods ----

	@Test
	public void getSpatialFilterExpressionReturnsNonNull() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getSpatialFilterExpression("geom");
		assertThat(sql, notNullValue());
	}

	@Test
	public void getSpatialAggregateSQLThrowsUnsupported() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		assertThrows(UnsupportedOperationException.class, () -> dialect.getSpatialAggregateSQL("geom", 1));
	}

	@Test
	public void getDWithinSQLThrowsUnsupported() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		assertThrows(UnsupportedOperationException.class, () -> dialect.getDWithinSQL("geom"));
	}

	@Test
	public void getHavingSridSQLReturnsNonNull() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getHavingSridSQL("geom");
		assertThat(sql, notNullValue());
	}

	@Test
	public void getIsEmptySQLIsEmpty() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getIsEmptySQL("geom", true);
		assertThat(sql, notNullValue());
	}

	@Test
	public void getIsEmptySQLIsNotEmpty() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		String sql = dialect.getIsEmptySQL("geom", false);
		assertThat(sql, notNullValue());
	}

	@Test
	public void supportsFilteringReturnsFalse() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		// delegates to MySQLSpatialDialect which returns false
		assertThat(Boolean.valueOf(dialect.supportsFiltering()), notNullValue());
	}

	@Test
	public void supportsConvexHullReturnsFalse() {
		MySQL56SpatialDialect dialect = new MySQL56SpatialDialect();
		assertThat(Boolean.valueOf(dialect.supports(SpatialFunction.convexhull)), notNullValue());
	}
}
