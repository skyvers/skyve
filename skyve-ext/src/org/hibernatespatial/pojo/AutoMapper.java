/**
 * $Id: AutoMapper.java 236 2010-07-29 21:49:00Z maesenka $
 *
 * This file is part of Hibernate Spatial, an extension to the 
 * hibernate ORM solution for geographic data. 
 *
 * Copyright © 2008 Geovise BVBA
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
package org.hibernatespatial.pojo;

import org.dom4j.Document;
import org.hibernatespatial.HBSpatialExtension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.SQLException;
import java.util.*;

/**
 * @author Karel Maesen, Geovise BVBA
 */
public class AutoMapper {

    protected final static Logger logger = LoggerFactory.getLogger(AutoMapper.class);

    protected final static String PACKAGE_NAME = "org.hibernatespatial.features.generated";

    private static Map<TableName, Class<?>> tableClassMap = new HashMap<TableName, Class<?>>();

    private static Map<TableName, ClassInfo> tableClassInfoMap = new HashMap<TableName, ClassInfo>();

    private static NamingStrategy naming = new SimpleNamingStrategy();

    /**
     * Returns the Hibernate mapping document for the specified tables
     * <p/>
     * <p>To create the mapping, a <code>Connection</code> object must be
     * provided to provide access to the specified tables.
     * This connection will not be closed on return.</p>
     *
     * @param conn       JDBC <code>Connection</code> used during mapping
     * @param catalog    database catalog
     * @param schema     database schema
     * @param tableNames list of table names
     * @return the XML mapping document that maps the tables specified by the catalog, schema and tablenames arguments.
     * @throws SQLException
     */
    public static synchronized Document map(Connection conn, String catalog, String schema, Collection<String> tableNames) throws SQLException {
        TypeMapper typeMapper = new TypeMapper(HBSpatialExtension.getDefaultSpatialDialect().getDbGeometryTypeName());
        DatabaseMetaData dmd = conn.getMetaData();
        FeatureMapper fMapper = new FeatureMapper(naming, typeMapper);
        FeatureClassGenerator fGenerator = new FeatureClassGenerator(PACKAGE_NAME, naming);

        for (String tableName : tableNames) {
            TableName table = new TableName(catalog, schema, tableName);
            if (tableClassInfoMap.get(table) != null) {
                logger.info("Class info for table " + tableName + " in catalog/schema " + catalog + "/" + schema + " has already been mapped.");
                continue;
            }
            logger.info("Generating class info for table " + tableName + " in catalog/schema " + catalog + "/" + schema);
            ClassInfo cInfo;
            try {
                cInfo = fMapper.createClassInfo(catalog, schema, tableName, dmd);
                logger.info("Generating class " + cInfo.getClassName() + " for table " + tableName);
                Class<?> clazz = fGenerator.generate(cInfo);
                tableClassMap.put(table, clazz);
                tableClassInfoMap.put(table, cInfo);

            } catch (TableNotFoundException e) {
                logger.warn(e.getMessage());
            } catch (MissingIdentifierException e) {
                logger.warn(e.getMessage());
            }
        }
        logger.info("Generating Hibernate Mapping file");
        MappingsGenerator mappingGenerator = new MappingsGenerator(PACKAGE_NAME);
        try {
            mappingGenerator.load(tableClassInfoMap.values(), schema);
        } catch (MissingIdentifierException e) {
            throw new RuntimeException(e);
        }
        return mappingGenerator.getMappingsDoc();
    }

    /**
     * Returns the <code>Class</code> object to which the specified table is mapped
     *
     * @param catalog   catalog of the table
     * @param schema    schema of the table
     * @param tableName name of the table
     * @return class to which the table specified by the arguments is mapped
     */
    public static Class<?> getClass(String catalog, String schema, String tableName) {
        TableName tbn = new TableName(catalog, schema, tableName);
        return tableClassMap.get(tbn);
    }

    /**
     * Returns the tables mapped by this automapper.
     *
     * @return a List of mapped tables. Each table is represented by a String array with the first
     *         component the catalog, the second the schema, and the third the table name.
     */
    public static List<String[]> getMappedTables() {
        List<String[]> list = new ArrayList<String[]>();
        for (TableName tbn : tableClassMap.keySet()) {
            String[] sa = new String[3];
            sa[0] = tbn.catalog;
            sa[1] = tbn.schema;
            sa[2] = tbn.tableName;
            list.add(sa);
        }
        return list;
    }

    /**
     * Returns the attribute names of the class to with the specified table is mapped
     *
     * @param catalog   catalog of the table
     * @param schema    schema of the table
     * @param tableName name of the table
     * @return list of attribute (field) names of the class that corresponds with the table identified by the arguments
     */
    public static List<String> getAttributes(String catalog, String schema, String tableName) {
        List<AttributeInfo> attributes = getAttributeInfos(catalog, schema, tableName);
        List<String> result = new ArrayList<String>();
        for (AttributeInfo attributeInfo : attributes) {
            result.add(attributeInfo.getFieldName());
        }
        return result;
    }

    private static List<AttributeInfo> getAttributeInfos(String catalog, String schema, String tableName) {
        TableName tbn = new TableName(catalog, schema, tableName);
        ClassInfo cInfo = tableClassInfoMap.get(tbn);
        if (cInfo == null) return new ArrayList<AttributeInfo>();
        return cInfo.getAttributes();
    }

    /**
     * Returns the Identifier attribute
     *
     * @param catalog   catalog of the table
     * @param schema    schema of the table
     * @param tableName name of the table
     * @return the attribute name which functions as a unique identifier for the objects corresponding
     * to rows in the specified table
     * @throws MissingIdentifierException when no Identifier property is available
     */
    public static String getIdAttribute(String catalog, String schema, String tableName) throws MissingIdentifierException {
        TableName tbn = new TableName(catalog, schema, tableName);
        ClassInfo cInfo = tableClassInfoMap.get(tbn);
        return cInfo.getIdAttribute().getFieldName();
    }

    /**
     * Returns the (default) <code>Geometry</code>-valued attribute
     *
     * @param catalog   catalog of the table
     * @param schema    schema of the table
     * @param tableName name of the table
     * @return the name of the <code>Geometry</code>-valued attribute
     * @throws GeometryNotFoundException when no <code>Geometry</code>-valued property is available
     */
    public static String getGeometryAttribute(String catalog, String schema, String tableName) throws GeometryNotFoundException {
        TableName tbn = new TableName(catalog, schema, tableName);
        ClassInfo cInfo = tableClassInfoMap.get(tbn);
        return cInfo.getGeomAttribute().getFieldName();
    }

    /**
     * Returns the name of the setter-method for the attribute
     *
     * @param catalog   catalog of the table
     * @param schema    schema of the table
     * @param tableName name of the table
     * @param attribute name of the attribute of the class to which this class is mapped
     * @return the name of the setter-method of the attribute specified by the arguments
     */
    public static String getAttributeSetterName(String catalog, String schema, String tableName, String attribute) {
        getAttributeInfo(catalog, schema, tableName, attribute);
        return naming.createSetterName(attribute);
    }

    /**
     * Returns the name of the getter-method for the attribute
     *
     * @param catalog   catalog of the table
     * @param schema    schema of the table
     * @param tableName name of the table
     * @param attribute name of the attribute of the class to which this class is mapped
     * @return the name of the getter-method of the attribute specified by the arguments
     *  */
    public static String getAttributeGetterName(String catalog, String schema, String tableName, String attribute) {
        getAttributeInfo(catalog, schema, tableName, attribute);
        return naming.createGetterName(attribute);
    }

    private static AttributeInfo getAttributeInfo(String catalog, String schema, String tableName, String attribute) {
        if (attribute == null) throw new IllegalArgumentException("Null attribute received.");
        for (AttributeInfo candidate : getAttributeInfos(catalog, schema, tableName)) {
            if (candidate.getFieldName().equals(attribute)) {
                return candidate;
            }
        }
        throw new IllegalArgumentException(String.format("%s is not an attribute of the class to which table %s is mapped.", attribute, tableName));
    }

    private static class TableName {
        String catalog;
        String schema;
        String tableName;

        private TableName(String catalog, String schema, String tableName) {
            this.catalog = catalog;
            this.schema = schema;
            this.tableName = tableName;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((catalog == null) ? 0 : catalog.hashCode());
            result = prime * result
                    + ((schema == null) ? 0 : schema.hashCode());
            result = prime * result
                    + ((tableName == null) ? 0 : tableName.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (!(obj instanceof TableName))
                return false;
            TableName other = (TableName) obj;
            if (catalog == null) {
                if (other.catalog != null)
                    return false;
            } else if (!catalog.equals(other.catalog))
                return false;
            if (schema == null) {
                if (other.schema != null)
                    return false;
            } else if (!schema.equals(other.schema))
                return false;
            if (tableName == null) {
                if (other.tableName != null)
                    return false;
            } else if (!tableName.equals(other.tableName))
                return false;
            return true;
        }


    }
}
