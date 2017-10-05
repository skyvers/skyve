package org.skyve.impl.persistence.hibernate.dialect;

import java.sql.Blob;
import java.sql.SQLException;

import org.geolatte.geom.codec.db.sqlserver.Decoders;
import org.geolatte.geom.codec.db.sqlserver.Encoders;
import org.geolatte.geom.jts.JTS;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.spatial.dialect.sqlserver.SqlServer2008GeometryTypeDescriptor;
import org.hibernate.spatial.dialect.sqlserver.SqlServer2008SpatialDialect;

import com.vividsolutions.jts.geom.Geometry;

public class SQLServer2008SpatialDialect extends SqlServer2008SpatialDialect implements SkyveDialect {
	private static final long serialVersionUID = 5463421110159264122L;

	private JTSGeometryType geometryType = new JTSGeometryType(SqlServer2008GeometryTypeDescriptor.INSTANCE);

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
	public String getModifyColumnString() {
		return "alter column";
	}
}
