package org.skyve.impl.persistence.hibernate.dialect;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;

@SuppressWarnings("static-method")
public class AbstractH2SpatialDialectTest {

	@Test
	public void getRDBMSReturnsH2() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.h2));
	}

	@Test
	public void getModifyColumnStringReturnsAlterColumn() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		assertThat(dialect.getModifyColumnString(), is("alter column"));
	}

	@Test
	public void getUniqueDelegateIsNotNull() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		assertThat(dialect.getUniqueDelegate(), notNullValue());
	}

	@Test
	public void getGeometrySqlTypeIsNonNegative() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		int sqlType = dialect.getGeometrySqlType();
		assertThat(Integer.valueOf(sqlType), notNullValue());
	}

	@Test
	public void getGeometryTypeIsNotNull() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	@Test
	public void isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		assertThat(Boolean.valueOf(dialect.isAlterTableColumnChangeRequired(null, null)), is(Boolean.FALSE));
	}

	@Test
	public void convertToPersistedValueReturnsNonNullForPoint() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		GeometryFactory gf = new GeometryFactory();
		Point point = gf.createPoint(new Coordinate(1.0, 2.0));
		Object result = dialect.convertToPersistedValue(point);
		assertNotNull(result);
	}

	@Test
	public void convertFromPersistedValueReturnsGeometryForEncodedPoint() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		GeometryFactory gf = new GeometryFactory();
		Point point = gf.createPoint(new Coordinate(1.0, 2.0));
		Object persisted = dialect.convertToPersistedValue(point);
		org.locationtech.jts.geom.Geometry restored = dialect.convertFromPersistedValue(persisted);
		assertNotNull(restored);
	}
}
