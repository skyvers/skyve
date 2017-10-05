package org.skyve.impl.persistence.hibernate.dialect;

import org.hibernate.spatial.JTSGeometryType;

import com.vividsolutions.jts.geom.Geometry;

public interface SkyveDialect {
	public int getGeometrySqlType();
	public JTSGeometryType getGeometryType();
	public Object convertToPersistedValue(Geometry geometry);
	public Geometry convertFromPersistedValue(Object geometry);
	
	public String getModifyColumnString();
}
