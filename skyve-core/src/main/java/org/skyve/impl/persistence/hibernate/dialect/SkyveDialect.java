package org.skyve.impl.persistence.hibernate.dialect;

import org.hibernate.mapping.Column;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;
import org.locationtech.jts.geom.Geometry;

/**
 * Skyve extension contract for Hibernate SQL dialects, adding spatial geometry
 * support and DDL introspection for schema management.
 *
 * <p>Each database-specific dialect implementation (H2, MySQL, PostgreSQL,
 * SQL Server) in {@code skyve-ext} implements this interface alongside its
 * Hibernate dialect base class, supplying:
 * <ul>
 *   <li>Geometry type registration and conversion between JTS {@link org.locationtech.jts.geom.Geometry}
 *       objects and the database's native spatial type.
 *   <li>{@link #isAlterTableColumnChangeRequired} — determines whether an existing
 *       column definition requires a DDL {@code ALTER} during schema evolution.
 *   <li>{@link #getModifyColumnString} — returns the platform-specific SQL fragment
 *       used to change a column definition (e.g. {@code MODIFY}, {@code ALTER COLUMN}).
 *   <li>{@link #getRDBMS} — identifies the underlying database platform via the
 *       {@link RDBMS} enum, allowing framework code to apply platform-specific logic.
 * </ul>
 */
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
