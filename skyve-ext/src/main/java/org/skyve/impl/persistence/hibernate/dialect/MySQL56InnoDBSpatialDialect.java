package org.skyve.impl.persistence.hibernate.dialect;

import org.hibernate.mapping.Column;
import org.hibernate.mapping.Index;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;
import org.hibernate.tool.schema.spi.Exporter;
import org.locationtech.jts.geom.Geometry;
import org.skyve.impl.persistence.hibernate.dialect.mysqlbugfix.MySQL56SpatialDialect;

public class MySQL56InnoDBSpatialDialect extends MySQL56SpatialDialect implements SkyveDialect {
	private static final long serialVersionUID = -6017668196369823514L;

	private MySQLSpatialDialectDelegate delegate = new MySQLSpatialDialectDelegate(this);

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
	
	@Override
	public Exporter<Index> getIndexExporter() {
		return delegate.getIndexExporter();
	}
}
