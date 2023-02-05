package org.skyve.impl.persistence.hibernate.dialect;

import java.io.Serializable;
import java.sql.SQLException;
import java.util.Iterator;

import org.geolatte.geom.ByteOrder;
import org.geolatte.geom.codec.Wkb;
import org.geolatte.geom.codec.WkbEncoder;
import org.geolatte.geom.jts.JTS;
import org.hibernate.boot.Metadata;
import org.hibernate.dialect.Dialect;
import org.hibernate.mapping.Column;
import org.hibernate.mapping.Index;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.spatial.dialect.postgis.PGGeometryTypeDescriptor;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;
import org.hibernate.tool.schema.internal.StandardIndexExporter;
import org.hibernate.tool.schema.spi.Exporter;
import org.locationtech.jts.geom.Geometry;
import org.postgresql.util.PGobject;

public final class PostgreSQLSpatialDialectDelegate implements SkyveDialect, Serializable {
	private static final long serialVersionUID = 614578841629775970L;

	private JTSGeometryType geometryType = new JTSGeometryType(PGGeometryTypeDescriptor.INSTANCE_WKB_2);
	private StandardIndexExporter indexExporter;

	public PostgreSQLSpatialDialectDelegate(Dialect dialect) {
		// We override index exporter for PostGreSQL so that we can specify lower() functional indexes
		// using text pattern index operations (except for PKs which can be case sensitive)
		indexExporter = new StandardIndexExporter(dialect) {
			@Override
			public String[] getSqlCreateStrings(Index index, Metadata metadata) {
				String[] result = super.getSqlCreateStrings(index, metadata);

				Iterator<Column> i = index.getColumnIterator();
				Column column = i.next();
				if (! i.hasNext()) { // 1 column - we can do something
					int typeCode = column.getSqlTypeCode(metadata);
					if (DDLDelegate.isChar(typeCode)) {
						String create = result[0];
						int openingBraceIndex = create.indexOf('(');
						create = new StringBuilder(create.length() + 25)
										.append(create.substring(0, openingBraceIndex + 1))
										.append("lower(")
										.append(create.substring(openingBraceIndex + 1, create.length() - 1))
										.append(") text_pattern_ops)")
										.toString();
						result[0] = create;
					}
				}

				return result;
			}
		};
	}
	
	@Override
	public int getGeometrySqlType() {
		return PGGeometryTypeDescriptor.INSTANCE_WKB_2.getSqlType();
	}

	@Override
	public JTSGeometryType getGeometryType() {
		return geometryType;
	}

	@Override
	public Object convertToPersistedValue(Geometry geometry) {
		final WkbEncoder encoder = Wkb.newEncoder(Wkb.Dialect.POSTGIS_EWKB_1);
		final String hexString = encoder.encode(JTS.from(geometry), ByteOrder.NDR).toString();
		final PGobject result = new PGobject();
		result.setType("geometry");
		try {
			result.setValue(hexString);
		}
		catch (SQLException e) {
			throw new IllegalArgumentException("Could not set valueon PGObject of " + hexString, e);
		}
		return result;
	}

	@Override
	public Geometry convertFromPersistedValue(Object geometry) {
		org.geolatte.geom.Geometry<?> result = PGGeometryTypeDescriptor.INSTANCE_WKB_2.toGeometry(geometry);
		return (result == null) ? null : JTS.to(result);
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
		return RDBMS.postgresql;
	}
	
	Exporter<Index> getIndexExporter() {
		return indexExporter;
	}
}
