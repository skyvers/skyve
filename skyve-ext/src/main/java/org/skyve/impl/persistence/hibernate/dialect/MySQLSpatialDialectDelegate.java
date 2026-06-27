package org.skyve.impl.persistence.hibernate.dialect;

import java.io.Serializable;
import java.sql.Types;
import java.util.Iterator;

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

/**
 * MySQL-specific {@link SkyveDialect} delegate that centralizes spatial conversion rules and
 * index DDL customizations used by Skyve.
 */
class MySQLSpatialDialectDelegate implements SkyveDialect, Serializable {
	private static final long serialVersionUID = -484304589859254284L;

	private JTSGeometryType geometryType = new JTSGeometryType(MySQLGeometryTypeDescriptor.INSTANCE);
	// This is used at startup (hopefully before any Serialization)
	private transient StandardIndexExporter indexExporter;

	/**
	 * Creates a helper that layers Skyve's MySQL-specific spatial and index behaviour over a Hibernate dialect.
	 *
	 * @param dialect the concrete Hibernate dialect that will emit index DDL
	 */
	public MySQLSpatialDialectDelegate(Dialect dialect) {
		// We override index exporter for MySQL so that we can specify the max length of indexes.
		// We use 1024 as we assume that innodb_large_prefix setting is ON - default in MySQL > 5.7.
		indexExporter = new StandardIndexExporter(dialect) {
			/**
			 * Builds index creation SQL and appends a prefix length for single large character columns
			 * when required by MySQL index length constraints.
			 *
			 * @param index the Hibernate index metadata
			 * @param metadata the Hibernate mapping metadata used to resolve SQL type codes
			 * @return SQL statements for creating the index
			 */
			@Override
			public String[] getSqlCreateStrings(Index index, Metadata metadata) {
				@SuppressWarnings("deprecation")
				String[] result = super.getSqlCreateStrings(index, metadata);

				Iterator<Column> i = index.getColumnIterator();
				Column column = i.next();
				if (! i.hasNext()) { // 1 column - we can do something
					int l = column.getLength();
					if (l >= 255) {
						int typeCode = column.getSqlTypeCode(metadata);
						if (DDLDelegate.isChar(typeCode)) {
							String create = result[0];
							create = new StringBuilder(create.length() + 6)
											.append(create.substring(0, create.length() - 1)).append('(').append(l).append("))")
											.toString();
							result[0] = create;
						}
					}
				}
				return result;
			}
		};
	}
	
	/**
	 * Returns the SQL type code used to persist geometry columns for this dialect.
	 *
	 * @return the JDBC SQL type code used for geometry persistence
	 */
	@Override
	public int getGeometrySqlType() {
		return MySQLGeometryTypeDescriptor.INSTANCE.getSqlType();
	}

	/**
	 * Returns the Hibernate geometry type descriptor used by this dialect.
	 *
	 * @return the geometry type descriptor bound to this dialect
	 */
	@Override
	public JTSGeometryType getGeometryType() {
		return geometryType;
	}

	// From MySQLGeometryTypeDescriptor
	/**
	 * Converts a JTS geometry value into the persisted database representation.
	 *
	 * @param geometry the geometry value to convert for persistence
	 * @return the converted value suitable for JDBC persistence
	 */
	@Override
	public Object convertToPersistedValue(Geometry geometry) {
		final WkbEncoder encoder = Wkb.newEncoder(Wkb.Dialect.MYSQL_WKB);
		final ByteBuffer buffer = encoder.encode(JTS.from(geometry), ByteOrder.NDR);
		return (buffer == null) ? null : buffer.toByteArray();
	}
	
	// From MySQLGeometryTypeDescriptor
	/**
	 * Converts a persisted geometry value into the JTS representation used by Skyve.
	 *
	 * @param geometry the persisted geometry value read from JDBC
	 * @return the converted JTS geometry value
	 */
	@Override
	public Geometry convertFromPersistedValue(Object geometry) {
		final ByteBuffer buffer = ByteBuffer.from((byte[]) geometry);
		final WkbDecoder decoder = Wkb.newDecoder(Wkb.Dialect.MYSQL_WKB);
		return JTS.to(decoder.decode(buffer));
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
		boolean result = DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo);
		// remove false positive where MySQL treats any varchar > 255 in length as various sizes of its text types.
		if (result && (columnInfo != null) && (columnInfo.getTypeCode() == Types.LONGVARCHAR) && column.getLength() >= 255) {
			result = false;
		}
		return result;
	}
	
	/**
	 * Returns the dialect-specific fragment used when altering an existing column definition.
	 *
	 * @return the SQL fragment used for alter-column statements
	 */
	@Override
	public String getModifyColumnString() {
		return "modify column";
	}
	
	/**
	 * Identifies the Skyve RDBMS family represented by this dialect.
	 *
	 * @return the Skyve RDBMS enum value for this dialect
	 */
	@Override
	public RDBMS getRDBMS() {
		return RDBMS.mysql;
	}
	
	/**
	 * Returns the index exporter used by this delegate when emitting index DDL.
	 *
	 * @return the configured MySQL-aware index exporter
	 */
	Exporter<Index> getIndexExporter() {
		return indexExporter;
	}
}
