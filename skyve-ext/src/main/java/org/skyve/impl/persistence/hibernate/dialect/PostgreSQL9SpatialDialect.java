package org.skyve.impl.persistence.hibernate.dialect;

import org.hibernate.mapping.Column;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.spatial.dialect.postgis.PostgisPG9Dialect;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;

import com.vividsolutions.jts.geom.Geometry;

public class PostgreSQL9SpatialDialect extends PostgisPG9Dialect implements SkyveDialect {
	private static final long serialVersionUID = 3464552138143768056L;

	private PostgreSQLSpatialDialectDelegate delegate = new PostgreSQLSpatialDialectDelegate();

	@Override
	public int getGeometrySqlType() {
		return delegate.getGeometrySqlType();
	}

	@Override
	public JTSGeometryType getGeometryType() {
		return delegate.getGeometryType();
	}

	@Override
	public Object convertToPersistedValue(Geometry geometry) {
		return delegate.convertToPersistedValue(geometry);
	}

	@Override
	public Geometry convertFromPersistedValue(Object geometry) {
		return delegate.convertFromPersistedValue(geometry);
	}

	@Override
	public boolean isAlterTableColumnChangeRequired(Column column, ColumnInformation columnInfo) {
		return delegate.isAlterTableColumnChangeRequired(column, columnInfo);
	}

	@Override
	public String getModifyColumnString() {
		return delegate.getModifyColumnString();
	}

	@Override
	public RDBMS getRDBMS() {
		return delegate.getRDBMS();
	}
}
