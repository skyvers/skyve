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

	@Override
	public int getGeometrySqlType() {
		return GeoDBGeometryTypeDescriptor.INSTANCE.getSqlType();
	}

	@Override
	public final JTSGeometryType getGeometryType() {
		return geometryType;
	}

	// From GeoDBGeometryTypeDescriptor
	@Override
	public Object convertToPersistedValue(Geometry geometry) {
		return GeoDbWkb.to(JTS.from(geometry));
	}

	// From GeoDBGeometryTypeDescriptor
	@Override
	public Geometry convertFromPersistedValue(Object geometry) {
		return JTS.to((org.geolatte.geom.Geometry<?>) GeoDbWkb.from(geometry));
	}

	@Override
	public boolean isAlterTableColumnChangeRequired(Column column, ColumnInformation columnInfo) {
		return DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo);
	}

	@Override
	public String getModifyColumnString() {
		return "alter column";
	}

	@Override
	public UniqueDelegate getUniqueDelegate() {
		return uniqueDelegate;
	}

	@Override
	public RDBMS getRDBMS() {
		return RDBMS.h2;
	}
}
