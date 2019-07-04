package org.skyve.impl.persistence.hibernate.dialect;

import java.io.Serializable;
import java.sql.Types;

import org.geolatte.geom.ByteBuffer;
import org.geolatte.geom.ByteOrder;
import org.geolatte.geom.codec.Wkb;
import org.geolatte.geom.codec.WkbDecoder;
import org.geolatte.geom.codec.WkbEncoder;
import org.geolatte.geom.jts.JTS;
import org.hibernate.boot.Metadata;
import org.hibernate.dialect.Dialect;
import org.hibernate.mapping.Column;
import org.hibernate.mapping.Index;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.spatial.dialect.mysql.MySQLGeometryTypeDescriptor;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;
import org.hibernate.tool.schema.internal.StandardIndexExporter;
import org.hibernate.tool.schema.spi.Exporter;
import org.locationtech.jts.geom.Geometry;

class MySQLSpatialDialectDelegate implements SkyveDialect, Serializable {
	private static final long serialVersionUID = -484304589859254284L;

	private JTSGeometryType geometryType = new JTSGeometryType(MySQLGeometryTypeDescriptor.INSTANCE);
	private StandardIndexExporter indexExporter;

	public MySQLSpatialDialectDelegate(Dialect dialect) {
		// We override index exporter for MySQL so that we can specify the max length of indexes
		// We use 1024 as we assume that innodb_large_prefix setting is ON - default in MySQL > 5.7.
		indexExporter = new StandardIndexExporter(dialect) {
			@Override
			public String[] getSqlCreateStrings(Index index, Metadata metadata) {
				String[] result = super.getSqlCreateStrings(index, metadata);
				for (int i = 0, l = result.length; i < l; i++) {
					String create = result[i];
					create = create.substring(0, create.length() - 1) + "(1024))";
					result[i] = create;
				}
				return result;
			}
		};
	}
	
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
	public boolean isAlterTableColumnChangeRequired(Column column, ColumnInformation columnInfo) {
		boolean result = DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo);
		// remove false positive where MySQL treats any varchar > 255 in length as various sizes of its text types.
		if (result && (columnInfo != null) && (columnInfo.getTypeCode() == Types.LONGVARCHAR) && column.getLength() >= 255) {
			result = false;
		}
		return result;
	}
	
	@Override
	public String getModifyColumnString() {
		return "modify column";
	}
	
	@Override
	public RDBMS getRDBMS() {
		return RDBMS.mysql;
	}
	
	Exporter<Index> getIndexExporter() {
		return indexExporter;
	}
}
