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

/**
 * A Dialect for MySQL 5 using InnoDB engine, with support for its spatial features
 *
 * @author Karel Maesen, Geovise BVBA
 */
public class MySQL5SpatialDialect extends MySQL5Dialect implements SpatialDialect {
	private static final long serialVersionUID = 3427528562034100060L;

	private MySQLSpatialDialect dialectDelegate = new MySQLSpatialDialect();

	/**
	 * Constructs an instance
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

	@Override
	public void contributeTypes(TypeContributions typeContributions, ServiceRegistry serviceRegistry) {
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

	@Override
	public String getSpatialRelateSQL(String columnName, int spatialRelation) {
		return dialectDelegate.getSpatialRelateSQL( columnName, spatialRelation );
	}

	@Override
	public String getSpatialFilterExpression(String columnName) {
		return dialectDelegate.getSpatialFilterExpression( columnName );
	}

	@Override
	public String getSpatialAggregateSQL(String columnName, int aggregation) {
		return dialectDelegate.getSpatialAggregateSQL( columnName, aggregation );
	}

	@Override
	public String getDWithinSQL(String columnName) {
		return dialectDelegate.getDWithinSQL( columnName );
	}

	@Override
	public String getHavingSridSQL(String columnName) {
		return dialectDelegate.getHavingSridSQL( columnName );
	}

	@Override
	public String getIsEmptySQL(String columnName, boolean isEmpty) {
		return dialectDelegate.getIsEmptySQL( columnName, isEmpty );
	}

	@Override
	public boolean supportsFiltering() {
		return dialectDelegate.supportsFiltering();
	}

	@Override
	public boolean supports(SpatialFunction function) {
		return dialectDelegate.supports( function );
	}

	@Override
	protected MySQLStorageEngine getDefaultMySQLStorageEngine() {
		return InnoDBStorageEngine.INSTANCE;
	}
}
