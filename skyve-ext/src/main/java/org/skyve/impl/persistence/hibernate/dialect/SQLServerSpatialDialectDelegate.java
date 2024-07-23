package org.skyve.impl.persistence.hibernate.dialect;

import java.io.Serializable;
import java.sql.Blob;
import java.sql.SQLException;

import org.geolatte.geom.codec.db.sqlserver.Decoders;
import org.geolatte.geom.codec.db.sqlserver.Encoders;
import org.geolatte.geom.jts.JTS;
import org.hibernate.dialect.Dialect;
import org.hibernate.dialect.unique.UniqueDelegate;
import org.hibernate.mapping.Column;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.spatial.dialect.sqlserver.SqlServer2008GeometryTypeDescriptor;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;
import org.locationtech.jts.geom.Geometry;

class SQLServerSpatialDialectDelegate implements SkyveDialect, Serializable {
	private static final long serialVersionUID = 880936968913130868L;

	private JTSGeometryType geometryType = new JTSGeometryType(SqlServer2008GeometryTypeDescriptor.INSTANCE);
	// This is used at startup (hopefully before any Serialization)
	private transient UniqueDelegate uniqueDelegate;

	public SQLServerSpatialDialectDelegate(Dialect actualDialect) {
		uniqueDelegate = new SQLServer2008NullTolerantUniqueDelegate(actualDialect);
	}
	
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
		return DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo);
	}
	
	@Override
	public String getModifyColumnString() {
		return "alter column";
	}
	
	@Override
	public RDBMS getRDBMS() {
		return RDBMS.sqlserver;
	}

	UniqueDelegate getUniqueDelegate() {
		return uniqueDelegate;
	}
}
