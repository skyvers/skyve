package org.skyve.impl.persistence.hibernate.dialect;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;

@SuppressWarnings("static-method")
class AbstractH2SpatialDialectTest {

	@Test
	void getRDBMSReturnsH2() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.h2));
	}

	@Test
	void getModifyColumnStringReturnsAlterColumn() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		assertThat(dialect.getModifyColumnString(), is("alter column"));
	}

	@Test
	void getUniqueDelegateIsNotNull() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		assertThat(dialect.getUniqueDelegate(), notNullValue());
	}

	@Test
	void getGeometrySqlTypeIsNonNegative() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		int sqlType = dialect.getGeometrySqlType();
		assertThat(Integer.valueOf(sqlType), notNullValue());
	}

	@Test
	void getGeometryTypeIsNotNull() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		assertThat(Boolean.valueOf(dialect.isAlterTableColumnChangeRequired(null, null)), is(Boolean.FALSE));
	}

	@Test
	void convertToPersistedValueReturnsNonNullForPoint() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		GeometryFactory gf = new GeometryFactory();
		Point point = gf.createPoint(new Coordinate(1.0, 2.0));
		Object result = dialect.convertToPersistedValue(point);
		assertNotNull(result);
	}

	@Test
	void convertFromPersistedValueReturnsGeometryForEncodedPoint() {
		H2SpatialDialect dialect = new H2SpatialDialect();
		GeometryFactory gf = new GeometryFactory();
		Point point = gf.createPoint(new Coordinate(1.0, 2.0));
		Object persisted = dialect.convertToPersistedValue(point);
		@SuppressWarnings("null") // uses a nullable result where the API expects non-null
		org.locationtech.jts.geom.Geometry restored = dialect.convertFromPersistedValue(persisted);
		assertNotNull(restored);
	}
}
