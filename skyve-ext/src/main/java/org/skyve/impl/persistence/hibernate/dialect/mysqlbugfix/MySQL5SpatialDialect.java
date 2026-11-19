/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * License: GNU Lesser General Public License (LGPL), version 2.1 or later.
 * See the lgpl.txt file in the root directory or <http://www.gnu.org/licenses/lgpl-2.1.html>.
 */
package org.skyve.impl.persistence.hibernate.dialect.mysqlbugfix;

import java.util.Map;

import org.hibernate.boot.model.TypeContributions;
import org.hibernate.dialect.InnoDBStorageEngine;
import org.hibernate.dialect.MySQL5Dialect;
import org.hibernate.dialect.MySQLStorageEngine;
import org.hibernate.dialect.function.SQLFunction;
import org.hibernate.service.ServiceRegistry;
import org.hibernate.spatial.SpatialDialect;
import org.hibernate.spatial.SpatialFunction;
import org.hibernate.spatial.dialect.mysql.MySQLGeometryTypeDescriptor;
import org.hibernate.spatial.dialect.mysql.MySQLSpatialDialect;

import jakarta.annotation.Nonnull;

/**
 * Provides the copied MySQL 5 spatial dialect variant used by Skyve's InnoDB wrapper dialects.
 */
public class MySQL5SpatialDialect extends MySQL5Dialect implements SpatialDialect {
	private static final long serialVersionUID = 3427528562034100060L;

	private MySQLSpatialDialect dialectDelegate = new MySQLSpatialDialect();

	/**
	 * Creates the MySQL 5 spatial dialect and registers the copied spatial functions.
	 */
	public MySQL5SpatialDialect() {
		super();
		registerColumnType(
				MySQLGeometryTypeDescriptor.INSTANCE.getSqlType(),
				"GEOMETRY"
		);
		for ( Map.Entry<String, SQLFunction> entry : new MySQL5SpatialFunctions() ) {
			registerFunction( entry.getKey(), entry.getValue() );
		}
	}

	/**
	 * Contributes MySQL spatial Hibernate types via the copied delegate dialect.
	 */
	@Override
	public void contributeTypes(@SuppressWarnings("null") @Nonnull TypeContributions typeContributions,
								@SuppressWarnings("null") @Nonnull ServiceRegistry serviceRegistry) {
		dialectDelegate.contributeTypes( typeContributions, serviceRegistry );
	}

// Comment these 2 methods out
// Symptom is that bizKeys (and everything over length 255 became longtext fields in the DB
//	@Override
//	public String getTypeName(int code, long length, int precision, int scale) throws HibernateException {
//		return dialectDelegate.getTypeName( code, length, precision, scale );
//	}
//
//	@Override
//	public SqlTypeDescriptor remapSqlTypeDescriptor(SqlTypeDescriptor sqlTypeDescriptor) {
//		return dialectDelegate.remapSqlTypeDescriptor( sqlTypeDescriptor );
//	}

	/**
	 * Returns the SQL predicate for the requested spatial relation.
	 */
	@Override
	public @Nonnull String getSpatialRelateSQL(@SuppressWarnings("null") @Nonnull String columnName, int spatialRelation) {
		return dialectDelegate.getSpatialRelateSQL( columnName, spatialRelation );
	}

	/**
	 * Returns the spatial filter expression for the supplied geometry column.
	 */
	@Override
	public @Nonnull String getSpatialFilterExpression(@SuppressWarnings("null") @Nonnull String columnName) {
		return dialectDelegate.getSpatialFilterExpression( columnName );
	}

	/**
	 * Returns SQL for the requested spatial aggregate operation.
	 */
	@Override
	public @Nonnull String getSpatialAggregateSQL(@SuppressWarnings("null") @Nonnull String columnName, int aggregation) {
		return dialectDelegate.getSpatialAggregateSQL( columnName, aggregation );
	}

	/**
	 * Returns SQL for distance-within comparisons against a geometry parameter.
	 */
	@Override
	public @Nonnull String getDWithinSQL(@SuppressWarnings("null") @Nonnull String columnName) {
		return dialectDelegate.getDWithinSQL( columnName );
	}

	/**
	 * Returns SQL that checks the SRID of the geometry column against a bind value.
	 */
	@Override
	public @Nonnull String getHavingSridSQL(@SuppressWarnings("null") @Nonnull String columnName) {
		return dialectDelegate.getHavingSridSQL( columnName );
	}

	/**
	 * Returns SQL that tests whether the geometry column is empty or non-empty.
	 */
	@Override
	public @Nonnull String getIsEmptySQL(@SuppressWarnings("null") @Nonnull String columnName, boolean isEmpty) {
		return dialectDelegate.getIsEmptySQL( columnName, isEmpty );
	}

	/**
	 * Indicates whether this dialect supports spatial filtering predicates.
	 */
	@Override
	public boolean supportsFiltering() {
		return dialectDelegate.supportsFiltering();
	}

	/**
	 * Indicates whether the given spatial function is supported by this dialect.
	 */
	@Override
	public boolean supports(@SuppressWarnings("null") @Nonnull SpatialFunction function) {
		return dialectDelegate.supports( function );
	}

	/**
	 * Returns the default MySQL storage engine used by this copied dialect.
	 *
	 * @return {@link InnoDBStorageEngine#INSTANCE}
	 */
	@Override
	protected @Nonnull MySQLStorageEngine getDefaultMySQLStorageEngine() {
		return InnoDBStorageEngine.INSTANCE;
	}
}
