package org.skyve.impl.persistence.hibernate.dialect;

import org.hibernate.mapping.Column;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;

import com.vividsolutions.jts.geom.Geometry;

public interface SkyveDialect {
	public static enum RDBMS {
		h2, mysql, sqlserver, postgresql
	}
	
	public int getGeometrySqlType();
	public JTSGeometryType getGeometryType();
	public Object convertToPersistedValue(Geometry geometry);
	public Geometry convertFromPersistedValue(Object geometry);
	
	public boolean isAlterTableColumnChangeRequired(Column column, ColumnInformation columnInfo);
	public String getModifyColumnString();
	
	public RDBMS getRDBMS();
}
