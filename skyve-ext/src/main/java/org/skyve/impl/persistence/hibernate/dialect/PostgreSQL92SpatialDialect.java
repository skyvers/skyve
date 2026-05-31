package org.skyve.impl.persistence.hibernate.dialect;

import org.hibernate.mapping.Column;
import org.hibernate.mapping.Index;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.spatial.dialect.postgis.PostgisPG92Dialect;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;
import org.hibernate.tool.schema.spi.Exporter;
import org.locationtech.jts.geom.Geometry;

/**
 * Skyve Hibernate dialect for PostgreSQL 9.2 with PostGIS spatial extensions.
 */
public class PostgreSQL92SpatialDialect extends PostgisPG92Dialect implements SkyveDialect {
	private static final long serialVersionUID = -2444385260980085754L;

	private PostgreSQLSpatialDialectDelegate delegate = new PostgreSQLSpatialDialectDelegate(this);

	/**
	 * Returns the SQL type code used to persist geometry columns for this dialect.
	 *
	 * @return the JDBC SQL type code used for geometry persistence
	 */
	@Override
	public int getGeometrySqlType() {
		return delegate.getGeometrySqlType();
	}

	/**
	 * Returns the Hibernate geometry type descriptor used by this dialect.
	 *
	 * @return the geometry type descriptor bound to this dialect
	 */
	@Override
	public JTSGeometryType getGeometryType() {
		return delegate.getGeometryType();
	}

	/**
	 * Converts a JTS geometry value into the persisted database representation.
	 *
	 * @param geometry the geometry value to convert for persistence
	 * @return the converted value suitable for JDBC persistence
	 */
	@Override
	public Object convertToPersistedValue(Geometry geometry) {
		return delegate.convertToPersistedValue(geometry);
	}

	/**
	 * Converts a persisted geometry value into the JTS representation used by Skyve.
	 *
	 * @param geometry the persisted geometry value read from JDBC
	 * @return the converted JTS geometry value
	 */
	@Override
	public Geometry convertFromPersistedValue(Object geometry) {
		return delegate.convertFromPersistedValue(geometry);
	}

	/**
	 * Determines whether Hibernate should emit column change DDL for the supplied column metadata.
	 *
	 * @param column the Hibernate column mapping definition
	 * @param columnInfo the extracted database column metadata
	 * @return true when column change DDL should be emitted, otherwise false
	 */
	@Override
	public boolean isAlterTableColumnChangeRequired(Column column, ColumnInformation columnInfo) {
		return delegate.isAlterTableColumnChangeRequired(column, columnInfo);
	}

	/**
	 * Returns the dialect-specific fragment used when altering an existing column definition.
	 *
	 * @return the SQL fragment used for alter-column statements
	 */
	@Override
	public String getModifyColumnString() {
		return delegate.getModifyColumnString();
	}

	/**
	 * Identifies the Skyve RDBMS family represented by this dialect.
	 *
	 * @return the Skyve RDBMS enum value for this dialect
	 */
	@Override
	public RDBMS getRDBMS() {
		return delegate.getRDBMS();
	}
	
	/**
	 * Returns the PostgreSQL index exporter that emits Skyve's functional index DDL.
	 *
	 * @return the index exporter used by this dialect
	 */
	@Override
	public Exporter<Index> getIndexExporter() {
		return delegate.getIndexExporter();
	}
}
