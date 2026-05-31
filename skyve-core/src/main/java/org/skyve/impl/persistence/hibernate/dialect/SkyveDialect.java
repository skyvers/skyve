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
	/**
	 * Identifies the supported relational database families that Skyve maps to
	 * concrete Hibernate dialect implementations.
	 */
	public static enum RDBMS {
		h2, mysql, sqlserver, postgresql
	}
	
	/**
	 * Returns the JDBC type code used when binding geometry columns for this dialect.
	 *
	 * @return the JDBC type code for persisted geometry values
	 */
	public int getGeometrySqlType();

	/**
	 * Returns the Hibernate type used to map persisted geometry values.
	 *
	 * @return the geometry type descriptor for this dialect
	 */
	public JTSGeometryType getGeometryType();

	/**
	 * Converts a JTS geometry into the database-specific value expected by Hibernate.
	 *
	 * @param geometry the geometry to persist; may be {@code null} when the caller is binding a nullable column
	 * @return the dialect-specific persisted representation
	 */
	public Object convertToPersistedValue(Geometry geometry);

	/**
	 * Converts a database geometry payload back into a JTS geometry instance.
	 *
	 * @param geometry the database value returned by JDBC or Hibernate
	 * @return the decoded geometry, or {@code null} when the database value is {@code null}
	 */
	public Geometry convertFromPersistedValue(Object geometry);
	
	/**
	 * Determines whether schema migration should emit an alter-column statement for the supplied column.
	 *
	 * @param column the mapped Hibernate column definition
	 * @param columnInfo the live database metadata for the column; may be {@code null} when the column does not yet exist
	 * @return {@code true} when the existing database column must be modified to match the mapping
	 */
	public boolean isAlterTableColumnChangeRequired(Column column, ColumnInformation columnInfo);
	
	/**
	 * Returns the vendor-specific SQL fragment used after {@code ALTER TABLE} when modifying a column definition.
	 *
	 * @return the dialect-specific column-modification fragment
	 */
	public String getModifyColumnString();
	
	/**
	 * Identifies the underlying relational database family for this dialect.
	 *
	 * @return the owning {@link RDBMS}
	 */
	public RDBMS getRDBMS();
}
