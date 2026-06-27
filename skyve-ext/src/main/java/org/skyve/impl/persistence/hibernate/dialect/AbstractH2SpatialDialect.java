package org.skyve.impl.persistence.hibernate.dialect;

import org.geolatte.geom.jts.JTS;
import org.hibernate.dialect.unique.UniqueDelegate;
import org.hibernate.mapping.Column;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.spatial.dialect.h2geodb.GeoDBDialect;
import org.hibernate.spatial.dialect.h2geodb.GeoDBGeometryTypeDescriptor;
import org.hibernate.spatial.dialect.h2geodb.GeoDbWkb;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;
import org.locationtech.jts.geom.Geometry;

/**
 * H2 1.3 and above dialect based on GeoDB that supports spatial.
 * The unique delegate is set in the constructor of subclasses.
 */
abstract class AbstractH2SpatialDialect extends GeoDBDialect implements SkyveDialect {
	private static final long serialVersionUID = 5082873409957781999L;

	static final int TEXT_MAX_LENGTH = 1000000000;

	private JTSGeometryType geometryType = new JTSGeometryType(GeoDBGeometryTypeDescriptor.INSTANCE);
	protected UniqueDelegate uniqueDelegate;

	/**
	 * Returns the SQL type code used to persist geometry columns for this dialect.
	 *
	 * @return the JDBC SQL type code used for geometry persistence
	 */
	@Override
	public int getGeometrySqlType() {
		return GeoDBGeometryTypeDescriptor.INSTANCE.getSqlType();
	}

	/**
	 * Returns the Hibernate geometry type descriptor used by this dialect.
	 *
	 * @return the geometry type descriptor bound to this dialect
	 */
	@Override
	public final JTSGeometryType getGeometryType() {
		return geometryType;
	}

	// From GeoDBGeometryTypeDescriptor
	/**
	 * Converts a JTS geometry value into the persisted database representation.
	 *
	 * @param geometry the geometry value to convert for persistence
	 * @return the converted value suitable for JDBC persistence
	 */
	@Override
	public Object convertToPersistedValue(Geometry geometry) {
		return GeoDbWkb.to(JTS.from(geometry));
	}

	// From GeoDBGeometryTypeDescriptor
	/**
	 * Converts a persisted geometry value into the JTS representation used by Skyve.
	 *
	 * @param geometry the persisted geometry value read from JDBC
	 * @return the converted JTS geometry value
	 */
	@Override
	public Geometry convertFromPersistedValue(Object geometry) {
		return JTS.to((org.geolatte.geom.Geometry<?>) GeoDbWkb.from(geometry));
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
		return DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo);
	}

	/**
	 * Returns the dialect-specific fragment used when altering an existing column definition.
	 *
	 * @return the SQL fragment used for alter-column statements
	 */
	@Override
	public String getModifyColumnString() {
		return "alter column";
	}

	/**
	 * Returns the unique-key delegate configured by concrete H2 dialect subclasses.
	 *
	 * @return the unique delegate used for unique-key DDL generation
	 */
	@Override
	public UniqueDelegate getUniqueDelegate() {
		return uniqueDelegate;
	}

	/**
	 * Identifies the Skyve RDBMS family represented by this dialect.
	 *
	 * @return the Skyve RDBMS enum value for this dialect
	 */
	@Override
	public RDBMS getRDBMS() {
		return RDBMS.h2;
	}
}
