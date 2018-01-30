package org.skyve.impl.persistence.hibernate.dialect;

import java.sql.Types;

import org.geolatte.geom.jts.JTS;
import org.hibernate.mapping.Column;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.spatial.dialect.h2geodb.GeoDBDialect;
import org.hibernate.spatial.dialect.h2geodb.GeoDBGeometryTypeDescriptor;
import org.hibernate.spatial.dialect.h2geodb.GeoDbWkb;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;

import com.vividsolutions.jts.geom.Geometry;

public class H2SpatialDialect extends GeoDBDialect implements SkyveDialect {
	private static final long serialVersionUID = 2491505869930720627L;

	/**
	 * Set the H2 Geometry SQL type to BLOB.
	 * It is set to GEOMETRY in the GeoDBDialect but this doesn't allow insert of JTS types.
	 */
	public H2SpatialDialect() {
		registerColumnType(GeoDBGeometryTypeDescriptor.INSTANCE.getSqlType(), "BLOB");
	}
	
	private JTSGeometryType geometryType = new JTSGeometryType(GeoDBGeometryTypeDescriptor.INSTANCE);

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
		boolean result = DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo);
		
		// Do additional check for varchar(Integer.MAX_VALUE) false positive.
		if (result) {
/*
			System.out.println("" + column.getSqlType() + " : " + 
								column.getSqlTypeCode() + " : " + 
								column.getLength() + " : " + 
								column.getPrecision() + " : " +
								column.getScale() + " : " + 
								column.getTypeIndex() + " = " +
								columnInfo.getColumnSize() + " : " +
								columnInfo.getDecimalDigits() + " : " + 
								columnInfo.getTypeCode() + " : " + 
								columnInfo.getTypeName() + " : " + 
								columnInfo.getColumnIdentifier());
*/
			if ((column.getLength() == 255) && 
					(column.getPrecision() == 19) && 
					(column.getScale() == 2) &&
					(column.getTypeIndex() == 0) &&
					(columnInfo.getColumnSize() == Integer.MAX_VALUE) &&
					(columnInfo.getDecimalDigits() == 0) &&
					(columnInfo.getTypeCode() == Types.VARCHAR)) {
				result = false;
			}
		}
		return result;
	}
	
	@Override
	public String getModifyColumnString() {
		return "alter column";
	}
}
