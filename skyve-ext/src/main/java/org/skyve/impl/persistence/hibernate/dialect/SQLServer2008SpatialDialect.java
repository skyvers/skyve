package org.skyve.impl.persistence.hibernate.dialect;

import org.hibernate.dialect.unique.UniqueDelegate;
import org.hibernate.mapping.Column;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.spatial.dialect.sqlserver.SqlServer2008SpatialDialect;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;
import org.locationtech.jts.geom.Geometry;

public class SQLServer2008SpatialDialect extends SqlServer2008SpatialDialect implements SkyveDialect {
	private static final long serialVersionUID = 5463421110159264122L;

	private SQLServerSpatialDialectDelegate delegate = new SQLServerSpatialDialectDelegate(this);
	
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
	public UniqueDelegate getUniqueDelegate() {
		return delegate.getUniqueDelegate();
	}
	
	@Override
	public RDBMS getRDBMS() {
		return delegate.getRDBMS();
	}
}
