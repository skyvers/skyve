package org.skyve.impl.persistence.hibernate.dialect;

import java.sql.Blob;
import java.sql.SQLException;
import java.sql.Types;

import org.geolatte.geom.codec.db.sqlserver.Decoders;
import org.geolatte.geom.codec.db.sqlserver.Encoders;
import org.geolatte.geom.jts.JTS;
import org.hibernate.dialect.unique.UniqueDelegate;
import org.hibernate.mapping.Column;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.spatial.dialect.sqlserver.SqlServer2008GeometryTypeDescriptor;
import org.hibernate.spatial.dialect.sqlserver.SqlServer2008SpatialDialect;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;

import com.vividsolutions.jts.geom.Geometry;

public class SQLServer2008SpatialDialect extends SqlServer2008SpatialDialect implements SkyveDialect {
	private static final long serialVersionUID = 5463421110159264122L;

	private JTSGeometryType geometryType = new JTSGeometryType(SqlServer2008GeometryTypeDescriptor.INSTANCE);
	private UniqueDelegate uniqueDelegate = new SQLServer2008NullTolerantUniqueDelegate(this);
	
	@Override
	public int getGeometrySqlType() {
		return SqlServer2008GeometryTypeDescriptor.INSTANCE.getSqlType();
	}

	@Override
	public JTSGeometryType getGeometryType() {
		return geometryType;
	}

	// From SqlServer2008GeometryTypeDescriptor
	@Override
	public Object convertToPersistedValue(Geometry geometry) {
		return Encoders.encode(JTS.from(geometry));
	}

	// From SqlServer2008GeometryTypeDescriptor
	@Override
	public Geometry convertFromPersistedValue(Object geometry) {
		byte[] bytes = null;
		if (geometry instanceof byte[]) {
			bytes = (byte[]) geometry;
		}
		else if (geometry instanceof Blob) {
			Blob blob = (Blob) geometry;
			try {
				bytes = blob.getBytes(1, (int) blob.length());
			}
			catch (SQLException e) {
				throw new IllegalArgumentException("Error on transforming blob into array.", e);
			}
		}
		else {
			throw new IllegalArgumentException( "Expected byte array or BLOB" );
		}
		return JTS.to((org.geolatte.geom.Geometry<?>) Decoders.decode(bytes));
	}

	@Override
	public boolean isAlterTableColumnChangeRequired(Column column, ColumnInformation columnInfo) {
		boolean result = DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo);
		
		// Do additional check for varchar(max) false positive.
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
	
	@Override
	public UniqueDelegate getUniqueDelegate() {
		return uniqueDelegate;
	}
}
