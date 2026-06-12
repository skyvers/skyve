package org.skyve.impl.persistence.hibernate.dialect;

import org.hibernate.dialect.unique.UniqueDelegate;
import org.hibernate.mapping.Column;
import org.hibernate.spatial.JTSGeometryType;
import org.hibernate.spatial.dialect.sqlserver.SqlServer2012SpatialDialect;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;
import org.locationtech.jts.geom.Geometry;

/**
 * Skyve Hibernate dialect for SQL Server 2012+ with spatial type support.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class SQLServer2012SpatialDialect extends SqlServer2012SpatialDialect implements SkyveDialect {
	private static final long serialVersionUID = -5092882478484022389L;

	private SQLServerSpatialDialectDelegate delegate = new SQLServerSpatialDialectDelegate(this);
	
	/**
	 * Returns the SQL type code used for geometry columns.
	 */
	@Override
	public int getGeometrySqlType() {
		return delegate.getGeometrySqlType();
	}

	/**
	 * Returns the Hibernate geometry type used by this dialect.
	 */
	@Override
	public JTSGeometryType getGeometryType() {
		return delegate.getGeometryType();
	}

	/**
	 * Converts a JTS geometry into the persisted SQL Server geometry representation.
	 */
	@Override
	public Object convertToPersistedValue(Geometry geometry) {
		return delegate.convertToPersistedValue(geometry);
	}

	/**
	 * Converts a persisted SQL Server geometry value back into a JTS geometry.
	 */
	@Override
	public Geometry convertFromPersistedValue(Object geometry) {
		return delegate.convertFromPersistedValue(geometry);
	}

	/**
	 * Determines whether a column definition difference requires ALTER COLUMN DDL.
	 */
	@Override
	public boolean isAlterTableColumnChangeRequired(Column column, ColumnInformation columnInfo) {
		return delegate.isAlterTableColumnChangeRequired(column, columnInfo);
	}
	
	/**
	 * Returns the SQL Server column-modification clause used in ALTER TABLE statements.
	 */
	@Override
	public String getModifyColumnString() {
		return delegate.getModifyColumnString();
	}
	
	/**
	 * Returns the SQL Server unique delegate that emits filtered unique-index DDL.
	 *
	 * @return the unique delegate used by this dialect
	 */
	@Override
	public UniqueDelegate getUniqueDelegate() {
		return delegate.getUniqueDelegate();
	}
	
	/**
	 * Returns the RDBMS identifier for this dialect.
	 */
	@Override
	public RDBMS getRDBMS() {
		return delegate.getRDBMS();
	}
}
