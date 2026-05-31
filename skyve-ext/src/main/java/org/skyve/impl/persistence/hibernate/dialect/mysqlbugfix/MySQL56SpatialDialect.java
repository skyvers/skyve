/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * License: GNU Lesser General Public License (LGPL), version 2.1 or later.
 * See the lgpl.txt file in the root directory or <http://www.gnu.org/licenses/lgpl-2.1.html>.
 */
package org.skyve.impl.persistence.hibernate.dialect.mysqlbugfix;

/**
 * @author Karel Maesen, Geovise BVBA
 * creation-date: 10/9/13
 */

import java.util.Map;

import org.hibernate.boot.model.TypeContributions;
import org.hibernate.dialect.MySQL55Dialect;
import org.hibernate.dialect.function.SQLFunction;
import org.hibernate.dialect.function.StandardSQLFunction;
import org.hibernate.service.ServiceRegistry;
import org.hibernate.spatial.SpatialDialect;
import org.hibernate.spatial.SpatialFunction;
import org.hibernate.spatial.SpatialRelation;
import org.hibernate.spatial.dialect.mysql.MySQLGeometryTypeDescriptor;
import org.hibernate.spatial.dialect.mysql.MySQLSpatialDialect;
import org.hibernate.type.StandardBasicTypes;

/**
 * Extends the MySQL5Dialect by including support for the spatial operators.
 * <p>
 * This <code>SpatialDialect</code> uses the ST_* spatial operators that operate on exact geometries which have been
 * added in MySQL version 5.6.1. Previous versions of MySQL only supported operators that operated on Minimum Bounding
 * Rectangles (MBR's). This dialect my therefore produce different results than the other MySQL spatial dialects.
 *
 * @author Karel Maesen
 */
public class MySQL56SpatialDialect extends MySQL55Dialect implements SpatialDialect {
	private static final long serialVersionUID = 1445850336151684170L;

	private MySQLSpatialDialect dialectDelegate = new MySQLSpatialDialect();

	/**
	 * Creates the MySQL 5.6 spatial dialect and registers the copied exact-geometry functions.
	 */
	public MySQL56SpatialDialect() {
		super();
		registerColumnType(
				MySQLGeometryTypeDescriptor.INSTANCE.getSqlType(),
				"GEOMETRY"
		);
		final MySQL5SpatialFunctions functionsToRegister = overrideObjectShapeFunctions( new MySQL5SpatialFunctions() );
		for ( Map.Entry<String, SQLFunction> entry : functionsToRegister ) {
			registerFunction( entry.getKey(), entry.getValue() );
		}
	}

	private static MySQL5SpatialFunctions overrideObjectShapeFunctions(MySQL5SpatialFunctions mysqlFunctions) {
		mysqlFunctions.put( "contains", new StandardSQLFunction( "ST_Contains", StandardBasicTypes.BOOLEAN ) );
		mysqlFunctions.put( "crosses", new StandardSQLFunction( "ST_Crosses", StandardBasicTypes.BOOLEAN ) );
		mysqlFunctions.put( "disjoint", new StandardSQLFunction( "ST_Disjoint", StandardBasicTypes.BOOLEAN ) );
		mysqlFunctions.put( "equals", new StandardSQLFunction( "ST_Equals", StandardBasicTypes.BOOLEAN ) );
		mysqlFunctions.put( "intersects", new StandardSQLFunction( "ST_Intersects", StandardBasicTypes.BOOLEAN ) );
		mysqlFunctions.put( "overlaps", new StandardSQLFunction( "ST_Overlaps", StandardBasicTypes.BOOLEAN ) );
		mysqlFunctions.put( "touches", new StandardSQLFunction( "ST_Touches", StandardBasicTypes.BOOLEAN ) );
		mysqlFunctions.put( "within", new StandardSQLFunction( "ST_Within", StandardBasicTypes.BOOLEAN ) );
		return mysqlFunctions;
	}

// Comment these 2 methods out
// Symptom is that bizKeys (and everything over length 255 became longtext fields in the DB
//	@Override
//	public SqlTypeDescriptor remapSqlTypeDescriptor(SqlTypeDescriptor sqlTypeDescriptor) {
//		return dialectDelegate.remapSqlTypeDescriptor( sqlTypeDescriptor );
//	}
//
//	@Override
//	public String getTypeName(int code, long length, int precision, int scale) throws HibernateException {
//		return dialectDelegate.getTypeName( code, length, precision, scale );
//	}

	/**
	 * Contributes Hibernate and spatial types for this MySQL 5.6-based dialect.
	 */
	@Override
	public void contributeTypes(
			TypeContributions typeContributions, ServiceRegistry serviceRegistry) {
		super.contributeTypes( typeContributions, serviceRegistry );
		dialectDelegate.contributeTypes( typeContributions, serviceRegistry );
	}

	/**
	 * Returns the SQL predicate for the requested spatial relation using MySQL 5.6 ST_* operators.
	 */
	@Override
	public String getSpatialRelateSQL(String columnName, int spatialRelation) {
		switch ( spatialRelation ) {
			case SpatialRelation.WITHIN:
				return " ST_Within(" + columnName + ",?)";
			case SpatialRelation.CONTAINS:
				return " ST_Contains(" + columnName + ", ?)";
			case SpatialRelation.CROSSES:
				return " ST_Crosses(" + columnName + ", ?)";
			case SpatialRelation.OVERLAPS:
				return " ST_Overlaps(" + columnName + ", ?)";
			case SpatialRelation.DISJOINT:
				return " ST_Disjoint(" + columnName + ", ?)";
			case SpatialRelation.INTERSECTS:
				return " ST_Intersects(" + columnName + ", ?)";
			case SpatialRelation.TOUCHES:
				return " ST_Touches(" + columnName + ", ?)";
			case SpatialRelation.EQUALS:
				return " ST_Equals(" + columnName + ", ?)";
			default:
				throw new IllegalArgumentException(
						"Spatial relation is not known by this dialect"
				);
		}
	}

	/**
	 * Returns the spatial filter expression for the supplied geometry column.
	 */
	@Override
	public String getSpatialFilterExpression(String columnName) {
		return dialectDelegate.getSpatialFilterExpression( columnName );
	}

	/**
	 * Returns SQL for the requested spatial aggregate operation.
	 */
	@Override
	public String getSpatialAggregateSQL(String columnName, int aggregation) {
		return dialectDelegate.getSpatialAggregateSQL( columnName, aggregation );
	}

	/**
	 * Returns SQL for distance-within comparisons against a geometry parameter.
	 */
	@Override
	public String getDWithinSQL(String columnName) {
		return dialectDelegate.getDWithinSQL( columnName );
	}

	/**
	 * Returns SQL that checks the SRID of the geometry column against a bind value.
	 */
	@Override
	public String getHavingSridSQL(String columnName) {
		return dialectDelegate.getHavingSridSQL( columnName );
	}

	/**
	 * Returns SQL that tests whether the geometry column is empty or non-empty.
	 */
	@Override
	public String getIsEmptySQL(String columnName, boolean isEmpty) {
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
	public boolean supports(SpatialFunction function) {
		return dialectDelegate.supports( function );
	}
}
