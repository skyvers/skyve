package org.skyve.impl.persistence.hibernate.dialect;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;

@SuppressWarnings("static-method")
public class PostgreSQLSpatialDialectTest {

	// ---- PostgreSQL9SpatialDialect ----

	@Test
	public void pg9getRDBMSReturnsPostgres() {
		PostgreSQL9SpatialDialect dialect = new PostgreSQL9SpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.postgresql));
	}

	@Test
	public void pg9getGeometrySqlTypeIsNonNegative() {
		PostgreSQL9SpatialDialect dialect = new PostgreSQL9SpatialDialect();
		int sqlType = dialect.getGeometrySqlType();
		assertThat(Integer.valueOf(sqlType), notNullValue());
	}

	@Test
	public void pg9getGeometryTypeIsNotNull() {
		PostgreSQL9SpatialDialect dialect = new PostgreSQL9SpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	@Test
	public void pg9getModifyColumnStringIsNotNull() {
		PostgreSQL9SpatialDialect dialect = new PostgreSQL9SpatialDialect();
		assertThat(dialect.getModifyColumnString(), notNullValue());
	}

	@Test
	public void pg9isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		PostgreSQL9SpatialDialect dialect = new PostgreSQL9SpatialDialect();
		assertFalse(dialect.isAlterTableColumnChangeRequired(null, null));
	}

	@Test
	public void pg9getIndexExporterIsNotNull() {
		PostgreSQL9SpatialDialect dialect = new PostgreSQL9SpatialDialect();
		assertThat(dialect.getIndexExporter(), notNullValue());
	}

	// ---- PostgreSQL91SpatialDialect ----

	@Test
	public void pg91getRDBMSReturnsPostgres() {
		PostgreSQL91SpatialDialect dialect = new PostgreSQL91SpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.postgresql));
	}

	@Test
	public void pg91getGeometrySqlTypeIsNonNegative() {
		PostgreSQL91SpatialDialect dialect = new PostgreSQL91SpatialDialect();
		int sqlType = dialect.getGeometrySqlType();
		assertThat(Integer.valueOf(sqlType), notNullValue());
	}

	@Test
	public void pg91getGeometryTypeIsNotNull() {
		PostgreSQL91SpatialDialect dialect = new PostgreSQL91SpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	// ---- PostgreSQL92SpatialDialect ----

	@Test
	public void pg92getRDBMSReturnsPostgres() {
		PostgreSQL92SpatialDialect dialect = new PostgreSQL92SpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.postgresql));
	}

	@Test
	public void pg92getGeometrySqlTypeIsNonNegative() {
		PostgreSQL92SpatialDialect dialect = new PostgreSQL92SpatialDialect();
		int sqlType = dialect.getGeometrySqlType();
		assertThat(Integer.valueOf(sqlType), notNullValue());
	}

	@Test
	public void pg92getGeometryTypeIsNotNull() {
		PostgreSQL92SpatialDialect dialect = new PostgreSQL92SpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	// ---- PostgreSQL82SpatialDialect ----

	@Test
	public void pg82getRDBMSReturnsPostgres() {
		PostgreSQL82SpatialDialect dialect = new PostgreSQL82SpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.postgresql));
	}

	@Test
	public void pg82getGeometryTypeIsNotNull() {
		PostgreSQL82SpatialDialect dialect = new PostgreSQL82SpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	@Test
	public void pg82isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		PostgreSQL82SpatialDialect dialect = new PostgreSQL82SpatialDialect();
		assertFalse(dialect.isAlterTableColumnChangeRequired(null, null));
	}

	@Test
	public void pg82getIndexExporterIsNotNull() {
		PostgreSQL82SpatialDialect dialect = new PostgreSQL82SpatialDialect();
		assertThat(dialect.getIndexExporter(), notNullValue());
	}

	// ---- PostgreSQL93SpatialDialect ----

	@Test
	public void pg93getRDBMSReturnsPostgres() {
		PostgreSQL93SpatialDialect dialect = new PostgreSQL93SpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.postgresql));
	}

	@Test
	public void pg93getGeometryTypeIsNotNull() {
		PostgreSQL93SpatialDialect dialect = new PostgreSQL93SpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	@Test
	public void pg93isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		PostgreSQL93SpatialDialect dialect = new PostgreSQL93SpatialDialect();
		assertFalse(dialect.isAlterTableColumnChangeRequired(null, null));
	}

	@Test
	public void pg93getIndexExporterIsNotNull() {
		PostgreSQL93SpatialDialect dialect = new PostgreSQL93SpatialDialect();
		assertThat(dialect.getIndexExporter(), notNullValue());
	}

	@Test
	public void pg93getGeometrySqlTypeIsNotNull() {
		PostgreSQL93SpatialDialect dialect = new PostgreSQL93SpatialDialect();
		assertThat(Integer.valueOf(dialect.getGeometrySqlType()), notNullValue());
	}

	// ---- PostgreSQL94SpatialDialect ----

	@Test
	public void pg94getRDBMSReturnsPostgres() {
		PostgreSQL94SpatialDialect dialect = new PostgreSQL94SpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.postgresql));
	}

	@Test
	public void pg94getGeometryTypeIsNotNull() {
		PostgreSQL94SpatialDialect dialect = new PostgreSQL94SpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	@Test
	public void pg94isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		PostgreSQL94SpatialDialect dialect = new PostgreSQL94SpatialDialect();
		assertFalse(dialect.isAlterTableColumnChangeRequired(null, null));
	}

	@Test
	public void pg94getIndexExporterIsNotNull() {
		PostgreSQL94SpatialDialect dialect = new PostgreSQL94SpatialDialect();
		assertThat(dialect.getIndexExporter(), notNullValue());
	}

	@Test
	public void pg94getGeometrySqlTypeIsNotNull() {
		PostgreSQL94SpatialDialect dialect = new PostgreSQL94SpatialDialect();
		assertThat(Integer.valueOf(dialect.getGeometrySqlType()), notNullValue());
	}

	// ---- PostgreSQL95SpatialDialect ----

	@Test
	public void pg95getRDBMSReturnsPostgres() {
		PostgreSQL95SpatialDialect dialect = new PostgreSQL95SpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.postgresql));
	}

	@Test
	public void pg95getGeometryTypeIsNotNull() {
		PostgreSQL95SpatialDialect dialect = new PostgreSQL95SpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	@Test
	public void pg95isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		PostgreSQL95SpatialDialect dialect = new PostgreSQL95SpatialDialect();
		assertFalse(dialect.isAlterTableColumnChangeRequired(null, null));
	}

	@Test
	public void pg95getIndexExporterIsNotNull() {
		PostgreSQL95SpatialDialect dialect = new PostgreSQL95SpatialDialect();
		assertThat(dialect.getIndexExporter(), notNullValue());
	}

	// ---- PostgreSQL10SpatialDialect ----

	@Test
	public void pg10getRDBMSReturnsPostgres() {
		PostgreSQL10SpatialDialect dialect = new PostgreSQL10SpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.postgresql));
	}

	@Test
	public void pg10getGeometryTypeIsNotNull() {
		PostgreSQL10SpatialDialect dialect = new PostgreSQL10SpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	@Test
	public void pg10isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		PostgreSQL10SpatialDialect dialect = new PostgreSQL10SpatialDialect();
		assertFalse(dialect.isAlterTableColumnChangeRequired(null, null));
	}

	@Test
	public void pg10getIndexExporterIsNotNull() {
		PostgreSQL10SpatialDialect dialect = new PostgreSQL10SpatialDialect();
		assertThat(dialect.getIndexExporter(), notNullValue());
	}

	// ---- PostgreSQL91SpatialDialect additional methods ----

	@Test
	public void pg91isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		PostgreSQL91SpatialDialect dialect = new PostgreSQL91SpatialDialect();
		assertFalse(dialect.isAlterTableColumnChangeRequired(null, null));
	}

	@Test
	public void pg91getIndexExporterIsNotNull() {
		PostgreSQL91SpatialDialect dialect = new PostgreSQL91SpatialDialect();
		assertThat(dialect.getIndexExporter(), notNullValue());
	}
	@Test
	public void pg95getGeometrySqlTypeIsNotNull() {
		PostgreSQL95SpatialDialect dialect = new PostgreSQL95SpatialDialect();
		assertThat(Integer.valueOf(dialect.getGeometrySqlType()), notNullValue());
	}
	// ---- PostgreSQL92SpatialDialect additional methods ----

	@Test
	public void pg92isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		PostgreSQL92SpatialDialect dialect = new PostgreSQL92SpatialDialect();
		assertFalse(dialect.isAlterTableColumnChangeRequired(null, null));
	}

	@Test
	public void pg92getIndexExporterIsNotNull() {
		PostgreSQL92SpatialDialect dialect = new PostgreSQL92SpatialDialect();
		assertThat(dialect.getIndexExporter(), notNullValue());
	}

	@Test
	public void pg91getModifyColumnStringIsNotNull() {
		PostgreSQL91SpatialDialect dialect = new PostgreSQL91SpatialDialect();
		assertThat(dialect.getModifyColumnString(), notNullValue());
	}

	@Test
	public void pg92getModifyColumnStringIsNotNull() {
		PostgreSQL92SpatialDialect dialect = new PostgreSQL92SpatialDialect();
		assertThat(dialect.getModifyColumnString(), notNullValue());
	}

	@Test
	public void pg82getModifyColumnStringIsNotNull() {
		PostgreSQL82SpatialDialect dialect = new PostgreSQL82SpatialDialect();
		assertThat(dialect.getModifyColumnString(), notNullValue());
	}

	@Test
	public void pg93getModifyColumnStringIsNotNull() {
		PostgreSQL93SpatialDialect dialect = new PostgreSQL93SpatialDialect();
		assertThat(dialect.getModifyColumnString(), notNullValue());
	}

	@Test
	public void pg94getModifyColumnStringIsNotNull() {
		PostgreSQL94SpatialDialect dialect = new PostgreSQL94SpatialDialect();
		assertThat(dialect.getModifyColumnString(), notNullValue());
	}

	@Test
	public void pg95getModifyColumnStringIsNotNull() {
		PostgreSQL95SpatialDialect dialect = new PostgreSQL95SpatialDialect();
		assertThat(dialect.getModifyColumnString(), notNullValue());
	}

	@Test
	public void pg10getGeometrySqlType() {
		PostgreSQL10SpatialDialect dialect = new PostgreSQL10SpatialDialect();
		int sqlType = dialect.getGeometrySqlType();
		assertThat(Integer.valueOf(sqlType), notNullValue());
	}

	@Test
	public void pg10getModifyColumnStringIsNotNull() {
		PostgreSQL10SpatialDialect dialect = new PostgreSQL10SpatialDialect();
		assertThat(dialect.getModifyColumnString(), notNullValue());
	}

	@Test
	public void pg9convertToPersistedValueReturnsNonNull() {
		PostgreSQL9SpatialDialect dialect = new PostgreSQL9SpatialDialect();
		GeometryFactory gf = new GeometryFactory();
		Point point = gf.createPoint(new Coordinate(1.0, 2.0));
		Object result = dialect.convertToPersistedValue(point);
		assertNotNull(result);
	}

	@Test
	public void pg9convertFromPersistedValueWithNullReturnsNull() {
		PostgreSQL9SpatialDialect dialect = new PostgreSQL9SpatialDialect();
		org.locationtech.jts.geom.Geometry result = dialect.convertFromPersistedValue(null);
		assertNull("null geometry should return null", result);
	}

        // ---- convertToPersistedValue for remaining dialects ----

        @Test
        public void pg82convertToPersistedValueReturnsNonNull() {
                PostgreSQL82SpatialDialect dialect = new PostgreSQL82SpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(1.0, 2.0));
                assertNotNull(dialect.convertToPersistedValue(point));
        }

        @Test
        public void pg82getGeometrySqlType() {
                PostgreSQL82SpatialDialect dialect = new PostgreSQL82SpatialDialect();
                assertThat(Integer.valueOf(dialect.getGeometrySqlType()), notNullValue());
        }

        @Test
        public void pg82convertFromPersistedValueWithNullReturnsNull() {
                PostgreSQL82SpatialDialect dialect = new PostgreSQL82SpatialDialect();
                assertNull(dialect.convertFromPersistedValue(null));
        }

        @Test
        public void pg91convertToPersistedValueReturnsNonNull() {
                PostgreSQL91SpatialDialect dialect = new PostgreSQL91SpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(1.0, 2.0));
                assertNotNull(dialect.convertToPersistedValue(point));
        }

        @Test
        public void pg91convertFromPersistedValueWithNullReturnsNull() {
                PostgreSQL91SpatialDialect dialect = new PostgreSQL91SpatialDialect();
                assertNull(dialect.convertFromPersistedValue(null));
        }

        @Test
        public void pg92convertToPersistedValueReturnsNonNull() {
                PostgreSQL92SpatialDialect dialect = new PostgreSQL92SpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(1.0, 2.0));
                assertNotNull(dialect.convertToPersistedValue(point));
        }

        @Test
        public void pg92convertFromPersistedValueWithNullReturnsNull() {
                PostgreSQL92SpatialDialect dialect = new PostgreSQL92SpatialDialect();
                assertNull(dialect.convertFromPersistedValue(null));
        }

        @Test
        public void pg93convertToPersistedValueReturnsNonNull() {
                PostgreSQL93SpatialDialect dialect = new PostgreSQL93SpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(1.0, 2.0));
                assertNotNull(dialect.convertToPersistedValue(point));
        }

        @Test
        public void pg93convertFromPersistedValueWithNullReturnsNull() {
                PostgreSQL93SpatialDialect dialect = new PostgreSQL93SpatialDialect();
                assertNull(dialect.convertFromPersistedValue(null));
        }

        @Test
        public void pg94convertToPersistedValueReturnsNonNull() {
                PostgreSQL94SpatialDialect dialect = new PostgreSQL94SpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(1.0, 2.0));
                assertNotNull(dialect.convertToPersistedValue(point));
        }

        @Test
        public void pg94convertFromPersistedValueWithNullReturnsNull() {
                PostgreSQL94SpatialDialect dialect = new PostgreSQL94SpatialDialect();
                assertNull(dialect.convertFromPersistedValue(null));
        }

        @Test
        public void pg95convertToPersistedValueReturnsNonNull() {
                PostgreSQL95SpatialDialect dialect = new PostgreSQL95SpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(1.0, 2.0));
                assertNotNull(dialect.convertToPersistedValue(point));
        }

        @Test
        public void pg95convertFromPersistedValueWithNullReturnsNull() {
                PostgreSQL95SpatialDialect dialect = new PostgreSQL95SpatialDialect();
                assertNull(dialect.convertFromPersistedValue(null));
        }

        @Test
        public void pg10convertToPersistedValueReturnsNonNull() {
                PostgreSQL10SpatialDialect dialect = new PostgreSQL10SpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(1.0, 2.0));
                assertNotNull(dialect.convertToPersistedValue(point));
        }

        @Test
        public void pg10convertFromPersistedValueWithNullReturnsNull() {
                PostgreSQL10SpatialDialect dialect = new PostgreSQL10SpatialDialect();
                assertNull(dialect.convertFromPersistedValue(null));
        }
}
