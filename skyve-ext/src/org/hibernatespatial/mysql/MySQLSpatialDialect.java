/**
 * $Id: MySQLSpatialDialect.java 298 2011-03-12 15:29:54Z maesenka $
 *
 * This file is part of Hibernate Spatial, an extension to the 
 * hibernate ORM solution for geographic data. 
 *
 * Copyright © 2007 Geovise BVBA
 * Copyright © 2007 K.U. Leuven LRD, Spatial Applications Division, Belgium
 *
 * This work was partially supported by the European Commission, 
 * under the 6th Framework Programme, contract IST-2-004688-STP.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * For more information, visit: http://www.hibernatespatial.org/
 */
package org.hibernatespatial.mysql;

import org.hibernate.Hibernate;
import org.hibernate.dialect.MySQLDialect;
import org.hibernate.dialect.function.StandardSQLFunction;
import org.hibernate.type.BinaryType;
import org.hibernate.type.BooleanType;
import org.hibernate.type.CustomType;
import org.hibernate.type.DoubleType;
import org.hibernate.type.IntegerType;
import org.hibernate.type.StringType;
import org.hibernate.type.Type;
import org.hibernate.usertype.UserType;
import org.hibernatespatial.SpatialDialect;
import org.hibernatespatial.SpatialFunction;
import org.hibernatespatial.SpatialRelation;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Extends the MySQLDialect by also including information on spatial operators,
 * constructors and processing functions.
 *
 * @author Karel Maesen
 * @author Boni Gopalan [3/11/2011:Refactored the code to introduce MySQLSpatialInnoDBDialect without much code duplication]
 */
public class MySQLSpatialDialect extends MySQLDialect implements SpatialDialect {


    private static final Type geometryCustomType = new CustomType(MySQLGeometryUserType.class, null);//, new String[]{"mysql_geometry"});

    protected Map<String, Integer> getColumnTypesToRegister() {
        Map<String, Integer> columnTypes = new HashMap<String, Integer>();
        columnTypes.put("GEOMETRY", Integer.valueOf(java.sql.Types.ARRAY));
        return columnTypes;
    }

    protected Map<String, StandardSQLFunction> getFunctionsToRegister() {
        Map<String, StandardSQLFunction> functionsToRegister = new HashMap<String, StandardSQLFunction>();

        // registering OGC functions
        // (spec_simplefeatures_sql_99-04.pdf)

        // section 2.1.1.1
        // Registerfunction calls for registering geometry functions:
        // first argument is the OGC standard functionname, second the name as
        // it occurs in the spatial dialect

        functionsToRegister.put("dimension", new StandardSQLFunction("dimension",
                Hibernate.INTEGER));
        functionsToRegister.put("geometrytype", new StandardSQLFunction(
                "geometrytype", Hibernate.STRING));
        functionsToRegister.put("srid", new StandardSQLFunction("srid",
                new IntegerType()));
        functionsToRegister.put("envelope", new StandardSQLFunction("envelope",
                new CustomType(MySQLGeometryUserType.class, null)));
        functionsToRegister.put("astext", new StandardSQLFunction("astext",
                new StringType()));
        functionsToRegister.put("asbinary", new StandardSQLFunction("asbinary",
                new BinaryType()));
        functionsToRegister.put("isempty", new StandardSQLFunction("isempty",
                new BooleanType()));
        functionsToRegister.put("boundary", new StandardSQLFunction("boundary",
                geometryCustomType));

        // Register functions for spatial relation constructs
        functionsToRegister.put("overlaps", new StandardSQLFunction("overlaps",
                new BooleanType()));
        functionsToRegister.put("intersects", new StandardSQLFunction("intersects",
        		new BooleanType()));
        functionsToRegister.put("equals", new StandardSQLFunction("equals",
        		new BooleanType()));
        functionsToRegister.put("contains", new StandardSQLFunction("contains",
        		new BooleanType()));
        functionsToRegister.put("crosses", new StandardSQLFunction("crosses",
        		new BooleanType()));
        functionsToRegister.put("disjoint", new StandardSQLFunction("disjoint",
        		new BooleanType()));
        functionsToRegister.put("touches", new StandardSQLFunction("touches",
        		new BooleanType()));
        functionsToRegister.put("within", new StandardSQLFunction("within",
        		new BooleanType()));
        functionsToRegister.put("relate", new StandardSQLFunction("relate",
        		new BooleanType()));

        // register the spatial analysis functions
        functionsToRegister.put("distance", new StandardSQLFunction("distance",
                new DoubleType()));
        functionsToRegister.put("buffer", new StandardSQLFunction("buffer",
                geometryCustomType));
        functionsToRegister.put("convexhull", new StandardSQLFunction("convexhull",
                geometryCustomType));
        functionsToRegister.put("difference", new StandardSQLFunction("difference",
                geometryCustomType));
        functionsToRegister.put("intersection", new StandardSQLFunction(
                "intersection", geometryCustomType));
        functionsToRegister.put("symdifference", new StandardSQLFunction(
                "symdifference", geometryCustomType));
        functionsToRegister.put("geomunion", new StandardSQLFunction("union",
                geometryCustomType));
        return functionsToRegister;
    }

    public MySQLSpatialDialect() {
        super();
        Map<String, StandardSQLFunction> functionsToRegister = getFunctionsToRegister();
        Map<String, Integer> columnTypes = getColumnTypesToRegister();
        if (null != columnTypes) {
            Iterator<String> keys = columnTypes.keySet().iterator();
            while (keys.hasNext()) {
                String aKey = keys.next();
                registerColumnType(columnTypes.get(aKey).intValue(), aKey);
            }
        }

        if (null != functionsToRegister) {
            Iterator<String> keys = functionsToRegister.keySet().iterator();
            while (keys.hasNext()) {
                String aKey = keys.next();
                registerFunction(aKey, functionsToRegister.get(aKey));

            }
        }
    }

    /**
     * @param columnName      The name of the geometry-typed column to which the relation is
     *                        applied
     * @param spatialRelation The type of spatial relation (as defined in
     *                        <code>SpatialRelation</code>).
     * @return
     */
    public String getSpatialRelateSQL(String columnName, int spatialRelation) {
        switch (spatialRelation) {
            case SpatialRelation.WITHIN:
                return " within(" + columnName + ",?)";
            case SpatialRelation.CONTAINS:
                return " contains(" + columnName + ", ?)";
            case SpatialRelation.CROSSES:
                return " crosses(" + columnName + ", ?)";
            case SpatialRelation.OVERLAPS:
                return " overlaps(" + columnName + ", ?)";
            case SpatialRelation.DISJOINT:
                return " disjoint(" + columnName + ", ?)";
            case SpatialRelation.INTERSECTS:
                return " intersects(" + columnName + ", ?)";
            case SpatialRelation.TOUCHES:
                return " touches(" + columnName + ", ?)";
            case SpatialRelation.EQUALS:
                return " equals(" + columnName + ", ?)";
            default:
                throw new IllegalArgumentException(
                        "Spatial relation is not known by this dialect");
        }

    }

    public String getSpatialFilterExpression(String columnName) {
        return "MBRIntersects(" + columnName + ", ? ) ";
    }

    /*
      * (non-Javadoc)
      *
      * @see org.hibernatespatial.SpatialDialect#getGeometryUserType()
      */

    public UserType getGeometryUserType() {
        return new MySQLGeometryUserType();
    }

    public String getSpatialAggregateSQL(String columnName, int aggregation) {
        throw new UnsupportedOperationException("Mysql has no spatial aggregate SQL functions.");
    }

    public String getDWithinSQL(String columnName) {
        throw new UnsupportedOperationException(String.format("Mysql doesn't support the Dwithin function"));
    }

    public String getHavingSridSQL(String columnName) {
        return " (srid(" + columnName + ") = ?) ";
    }

    public String getIsEmptySQL(String columnName, boolean isEmpty) {
        String emptyExpr = " IsEmpty(" + columnName + ") ";
        return isEmpty ? emptyExpr : "( NOT " + emptyExpr + ")";
    }

    public String getDbGeometryTypeName() {
        return "GEOMETRY";
    }


    public boolean isTwoPhaseFiltering() {
        return false;
    }

    public boolean supportsFiltering() {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean supports(SpatialFunction function) {
        switch (function) {
            case boundary:
            case relate:
            case distance:
            case buffer:
            case convexhull:
            case difference:
            case symdifference:
            case intersection:
            case geomunion:
            case dwithin:
            case transform:
                return false;
        }
        return true;
    }

}
