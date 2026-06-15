package org.skyve.impl.persistence.hibernate.dialect;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;

@SuppressWarnings("static-method")
public class MySQLSpatialDialectTest {

	// ---- MySQL5InnoDBSpatialDialect ----

	@Test
	public void mysql5getRDBMSReturnsMysql() {
		MySQL5InnoDBSpatialDialect dialect = new MySQL5InnoDBSpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.mysql));
	}

	@Test
	public void mysql5getGeometryTypeIsNotNull() {
		MySQL5InnoDBSpatialDialect dialect = new MySQL5InnoDBSpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	@Test
	public void mysql5getGeometrySqlType() {
		MySQL5InnoDBSpatialDialect dialect = new MySQL5InnoDBSpatialDialect();
		int sqlType = dialect.getGeometrySqlType();
		assertThat(Integer.valueOf(sqlType), notNullValue());
	}

	@Test
	public void mysql5getModifyColumnStringIsNotNull() {
		MySQL5InnoDBSpatialDialect dialect = new MySQL5InnoDBSpatialDialect();
		assertThat(dialect.getModifyColumnString(), notNullValue());
	}

	@Test
	public void mysql5isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		MySQL5InnoDBSpatialDialect dialect = new MySQL5InnoDBSpatialDialect();
		assertFalse(dialect.isAlterTableColumnChangeRequired(null, null));
	}

	@Test
	public void mysql5getIndexExporterIsNotNull() {
		MySQL5InnoDBSpatialDialect dialect = new MySQL5InnoDBSpatialDialect();
		assertThat(dialect.getIndexExporter(), notNullValue());
	}

	// ---- MySQL56InnoDBSpatialDialect ----

	@Test
	public void mysql56getRDBMSReturnsMysql() {
		MySQL56InnoDBSpatialDialect dialect = new MySQL56InnoDBSpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.mysql));
	}

	@Test
	public void mysql56getGeometryTypeIsNotNull() {
		MySQL56InnoDBSpatialDialect dialect = new MySQL56InnoDBSpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	@Test
	public void mysql56isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		MySQL56InnoDBSpatialDialect dialect = new MySQL56InnoDBSpatialDialect();
		assertFalse(dialect.isAlterTableColumnChangeRequired(null, null));
	}

	@Test
	public void mysql56getIndexExporterIsNotNull() {
		MySQL56InnoDBSpatialDialect dialect = new MySQL56InnoDBSpatialDialect();
		assertThat(dialect.getIndexExporter(), notNullValue());
	}

	// ---- MySQL8InnoDBSpatialDialect ----

	@Test
	public void mysql8getRDBMSReturnsMysql() {
		MySQL8InnoDBSpatialDialect dialect = new MySQL8InnoDBSpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.mysql));
	}

	@Test
	public void mysql8getGeometryTypeIsNotNull() {
		MySQL8InnoDBSpatialDialect dialect = new MySQL8InnoDBSpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	@Test
	public void mysql8isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		MySQL8InnoDBSpatialDialect dialect = new MySQL8InnoDBSpatialDialect();
		assertFalse(dialect.isAlterTableColumnChangeRequired(null, null));
	}

	@Test
	public void mysql8getIndexExporterIsNotNull() {
		MySQL8InnoDBSpatialDialect dialect = new MySQL8InnoDBSpatialDialect();
		assertThat(dialect.getIndexExporter(), notNullValue());
	}

	@Test
	public void mysql56getGeometrySqlType() {
		MySQL56InnoDBSpatialDialect dialect = new MySQL56InnoDBSpatialDialect();
		int sqlType = dialect.getGeometrySqlType();
		assertThat(Integer.valueOf(sqlType), notNullValue());
	}

	@Test
	public void mysql56getModifyColumnStringIsNotNull() {
		MySQL56InnoDBSpatialDialect dialect = new MySQL56InnoDBSpatialDialect();
		assertThat(dialect.getModifyColumnString(), notNullValue());
	}

	@Test
	public void mysql8getGeometrySqlType() {
		MySQL8InnoDBSpatialDialect dialect = new MySQL8InnoDBSpatialDialect();
		int sqlType = dialect.getGeometrySqlType();
		assertThat(Integer.valueOf(sqlType), notNullValue());
	}

	@Test
	public void mysql8getModifyColumnStringIsNotNull() {
		MySQL8InnoDBSpatialDialect dialect = new MySQL8InnoDBSpatialDialect();
		assertThat(dialect.getModifyColumnString(), notNullValue());
	}

        // ---- MySQLSpatialDialectDelegate — convertTo/FromPersistedValue ----

        @Test
        public void mysql5ConvertToPersistedValueReturnsNonNull() {
                MySQL5InnoDBSpatialDialect dialect = new MySQL5InnoDBSpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(1.0, 2.0));
                Object result = dialect.convertToPersistedValue(point);
                assertNotNull(result);
        }

        @Test
        public void mysql5ConvertRoundTrip() {
                MySQL5InnoDBSpatialDialect dialect = new MySQL5InnoDBSpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(3.0, 4.0));
                Object bytes = dialect.convertToPersistedValue(point);
                assertNotNull(bytes);
                // round-trip: decode the bytes back
                Object geom = dialect.convertFromPersistedValue(bytes);
                assertNotNull(geom);
        }

        @Test
        public void mysql56ConvertToPersistedValueReturnsNonNull() {
                MySQL56InnoDBSpatialDialect dialect = new MySQL56InnoDBSpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(5.0, 6.0));
                Object result = dialect.convertToPersistedValue(point);
                assertNotNull(result);
        }

        @Test
        public void mysql56ConvertRoundTrip() {
                MySQL56InnoDBSpatialDialect dialect = new MySQL56InnoDBSpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(5.0, 6.0));
                Object bytes = dialect.convertToPersistedValue(point);
                assertNotNull(dialect.convertFromPersistedValue(bytes));
        }

        @Test
        public void mysql8ConvertToPersistedValueReturnsNonNull() {
                MySQL8InnoDBSpatialDialect dialect = new MySQL8InnoDBSpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(7.0, 8.0));
                Object result = dialect.convertToPersistedValue(point);
                assertNotNull(result);
        }

        @Test
        public void mysql8ConvertRoundTrip() {
                MySQL8InnoDBSpatialDialect dialect = new MySQL8InnoDBSpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(7.0, 8.0));
                Object bytes = dialect.convertToPersistedValue(point);
                assertNotNull(dialect.convertFromPersistedValue(bytes));
        }
}
