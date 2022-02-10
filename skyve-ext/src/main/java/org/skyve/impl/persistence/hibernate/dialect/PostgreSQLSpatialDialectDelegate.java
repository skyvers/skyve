package org.skyve.impl.persistence.hibernate.dialect;

import java.io.Serializable;
import java.sql.SQLException;

import org.geolatte.geom.ByteOrder;
import org.geolatte.geom.codec.Wkb;
import org.geolatte.geom.codec.WkbEncoder;
import org.geolatte.geom.jts.JTS;
import org.hibernate.mapping.Column;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.spatial.dialect.postgis.PGGeometryTypeDescriptor;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;
import org.locationtech.jts.geom.Geometry;
import org.postgresql.util.PGobject;

public final class PostgreSQLSpatialDialectDelegate implements SkyveDialect, Serializable {
	private static final long serialVersionUID = 614578841629775970L;

	private JTSGeometryType geometryType = new JTSGeometryType(PGGeometryTypeDescriptor.INSTANCE_WKB_2);

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
		final WkbEncoder encoder = Wkb.newEncoder( Wkb.Dialect.POSTGIS_EWKB_1 );
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
}
