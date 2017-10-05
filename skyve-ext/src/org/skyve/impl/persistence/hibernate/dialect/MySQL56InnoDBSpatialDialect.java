package org.skyve.impl.persistence.hibernate.dialect;

import org.geolatte.geom.ByteBuffer;
import org.geolatte.geom.ByteOrder;
import org.geolatte.geom.codec.Wkb;
import org.geolatte.geom.codec.WkbDecoder;
import org.geolatte.geom.codec.WkbEncoder;
import org.geolatte.geom.jts.JTS;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.spatial.dialect.mysql.MySQLGeometryTypeDescriptor;

import com.vividsolutions.jts.geom.Geometry;

@SuppressWarnings("deprecation")
public class MySQL56InnoDBSpatialDialect extends org.hibernate.spatial.dialect.mysql.MySQL56InnoDBSpatialDialect implements SkyveDialect {
	private static final long serialVersionUID = -6017668196369823514L;

	private JTSGeometryType geometryType = new JTSGeometryType(MySQLGeometryTypeDescriptor.INSTANCE);

	@Override
	public int getGeometrySqlType() {
		return MySQLGeometryTypeDescriptor.INSTANCE.getSqlType();
	}

	@Override
	public JTSGeometryType getGeometryType() {
		return geometryType;
	}

	// From MySQLGeometryTypeDescriptor
	@Override
	public Object convertToPersistedValue(Geometry geometry) {
		final WkbEncoder encoder = Wkb.newEncoder(Wkb.Dialect.MYSQL_WKB);
		final ByteBuffer buffer = encoder.encode(JTS.from(geometry), ByteOrder.NDR);
		return (buffer == null) ? null : buffer.toByteArray();
	}
	
	// From MySQLGeometryTypeDescriptor
	@Override
	public Geometry convertFromPersistedValue(Object geometry) {
		final ByteBuffer buffer = ByteBuffer.from((byte[]) geometry);
		final WkbDecoder decoder = Wkb.newDecoder(Wkb.Dialect.MYSQL_WKB);
		return JTS.to(decoder.decode(buffer));
	}

	@Override
	public String getModifyColumnString() {
		return "modify column";
	}
}
