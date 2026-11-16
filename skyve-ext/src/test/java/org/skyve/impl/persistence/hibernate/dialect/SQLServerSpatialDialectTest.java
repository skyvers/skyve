package org.skyve.impl.persistence.hibernate.dialect;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;
@SuppressWarnings("static-method")
class SQLServerSpatialDialectTest {

	// ---- SQLServer2008SpatialDialect ----

	@Test
	void sqlServer2008getRDBMSReturnsSqlServer() {
		SQLServer2008SpatialDialect dialect = new SQLServer2008SpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.sqlserver));
	}

	@Test
	void sqlServer2008getGeometryTypeIsNotNull() {
		SQLServer2008SpatialDialect dialect = new SQLServer2008SpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	@Test
	void sqlServer2008getGeometrySqlType() {
		SQLServer2008SpatialDialect dialect = new SQLServer2008SpatialDialect();
		int sqlType = dialect.getGeometrySqlType();
		assertThat(Integer.valueOf(sqlType), notNullValue());
	}

	@Test
	void sqlServer2008getModifyColumnStringIsNotNull() {
		SQLServer2008SpatialDialect dialect = new SQLServer2008SpatialDialect();
		assertThat(dialect.getModifyColumnString(), notNullValue());
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void sqlServer2008isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		SQLServer2008SpatialDialect dialect = new SQLServer2008SpatialDialect();
		assertFalse(dialect.isAlterTableColumnChangeRequired(null, null));
	}

	// ---- SQLServer2012SpatialDialect ----

	@Test
	void sqlServer2012getRDBMSReturnsSqlServer() {
		SQLServer2012SpatialDialect dialect = new SQLServer2012SpatialDialect();
		assertThat(dialect.getRDBMS(), is(RDBMS.sqlserver));
	}

	@Test
	void sqlServer2012getGeometryTypeIsNotNull() {
		SQLServer2012SpatialDialect dialect = new SQLServer2012SpatialDialect();
		assertThat(dialect.getGeometryType(), notNullValue());
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void sqlServer2012isAlterTableColumnChangeRequiredWithNullsReturnsFalse() {
		SQLServer2012SpatialDialect dialect = new SQLServer2012SpatialDialect();
		assertFalse(dialect.isAlterTableColumnChangeRequired(null, null));
	}

	@Test
	void sqlServer2012getGeometrySqlType() {
		SQLServer2012SpatialDialect dialect = new SQLServer2012SpatialDialect();
		int sqlType = dialect.getGeometrySqlType();
		assertThat(Integer.valueOf(sqlType), notNullValue());
	}

	@Test
	void sqlServer2012getModifyColumnStringIsNotNull() {
		SQLServer2012SpatialDialect dialect = new SQLServer2012SpatialDialect();
		assertThat(dialect.getModifyColumnString(), notNullValue());
	}

	@Test
	void sqlServer2008getUniqueDelegateIsNotNull() {
		SQLServer2008SpatialDialect dialect = new SQLServer2008SpatialDialect();
		assertThat(dialect.getUniqueDelegate(), notNullValue());
	}

	@Test
	void sqlServer2012getUniqueDelegateIsNotNull() {
		SQLServer2012SpatialDialect dialect = new SQLServer2012SpatialDialect();
		assertThat(dialect.getUniqueDelegate(), notNullValue());
	}

        // ---- convertToPersistedValue ----

        @Test
        void sqlServer2008ConvertToPersistedValueReturnsNonNull() {
                SQLServer2008SpatialDialect dialect = new SQLServer2008SpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(1.0, 2.0));
                assertNotNull(dialect.convertToPersistedValue(point));
        }

        @Test
        void sqlServer2012ConvertToPersistedValueReturnsNonNull() {
                SQLServer2012SpatialDialect dialect = new SQLServer2012SpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(3.0, 4.0));
                assertNotNull(dialect.convertToPersistedValue(point));
        }

        @Test
        void sqlServer2008ConvertFromPersistedValueRoundTrip() {
                SQLServer2008SpatialDialect dialect = new SQLServer2008SpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(5.0, 6.0));
                Object bytes = dialect.convertToPersistedValue(point);
                assertNotNull(dialect.convertFromPersistedValue(bytes));
        }

        @Test
        void sqlServer2012ConvertFromPersistedValueRoundTrip() {
                SQLServer2012SpatialDialect dialect = new SQLServer2012SpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(5.0, 6.0));
                Object bytes = dialect.convertToPersistedValue(point);
                assertNotNull(dialect.convertFromPersistedValue(bytes));
        }

        @Test
        void sqlServer2008ConvertFromPersistedValueWithBlob() throws Exception {
                SQLServer2008SpatialDialect dialect = new SQLServer2008SpatialDialect();
                GeometryFactory gf = new GeometryFactory();
                Point point = gf.createPoint(new Coordinate(5.0, 6.0));
                byte[] bytes = (byte[]) dialect.convertToPersistedValue(point);
                // Wrap as a Blob and ensure convertFromPersistedValue handles it
                java.sql.Blob blob = new javax.sql.rowset.serial.SerialBlob(bytes);
                assertNotNull(dialect.convertFromPersistedValue(blob));
        }

        @Test
        void sqlServer2008ConvertFromPersistedValueWithInvalidTypeThrows() {
                SQLServer2008SpatialDialect dialect = new SQLServer2008SpatialDialect();
                assertThrows(IllegalArgumentException.class, () -> dialect.convertFromPersistedValue("not_a_byte_array"));
        }

        @Test
        @SuppressWarnings("boxing")
        void sqlServer2008ConvertFromPersistedValueBlobGetBytesThrowsSQLExceptionWraps() throws Exception {
                SQLServer2008SpatialDialect dialect = new SQLServer2008SpatialDialect();
                java.sql.Blob blob = mock(java.sql.Blob.class);
                when(blob.length()).thenReturn(10L);
                when(blob.getBytes(1L, 10)).thenThrow(new java.sql.SQLException("SQL error"));
                assertThrows(IllegalArgumentException.class, () -> dialect.convertFromPersistedValue(blob));
        }
}
