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

public class H2SpatialDialect extends GeoDBDialect implements SkyveDialect {
	private static final long serialVersionUID = 2491505869930720627L;

	static final int TEXT_MAX_LENGTH = 1000000000;
	
	private JTSGeometryType geometryType = new JTSGeometryType(GeoDBGeometryTypeDescriptor.INSTANCE);
	private UniqueDelegate uniqueDelegate = new H2NoOpUniqueDelegate(this);
	
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
