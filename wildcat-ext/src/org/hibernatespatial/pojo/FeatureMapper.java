package org.hibernatespatial.pojo;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class FeatureMapper {

    private final static Logger LOGGER = LoggerFactory.getLogger(FeatureMapper.class);

    private final NamingStrategy naming;
    private final TypeMapper typeMapper;

    public FeatureMapper(NamingStrategy naming, TypeMapper typeMapper) {
        this.naming = naming;
        this.typeMapper = typeMapper;
    }

    public ClassInfo createClassInfo(String catalog, String schema, String tableName, DatabaseMetaData dmd) throws TableNotFoundException, MissingIdentifierException {
        String className = naming.createClassName(tableName);
        ClassInfo cInfo = new ClassInfo(tableName, className);
        readColums(catalog, schema, tableName, dmd, cInfo);
        determineIdentifier(catalog, schema, tableName, dmd, cInfo);
        return cInfo;
    }

    private void determineIdentifier(String catalog, String schema, String tableName, DatabaseMetaData dmd, ClassInfo cInfo) throws MissingIdentifierException {
        String pkn = null;
        pkn = determinePrimaryKey(catalog, schema, tableName, dmd);
        if (pkn == null) {
            pkn = findUniqueIndex(catalog, schema, tableName, dmd);
        }
        if (pkn == null) throw new MissingIdentifierException(tableName);
        setAsIdentifier(cInfo, pkn);
        return;

    }

    private String findUniqueIndex(String catalog, String schema, String tableName, DatabaseMetaData dmd) {
        Map<String, String> indexes = new HashMap<String, String>();
        Set<String> rejectedIndexes = new HashSet<String>();
        readUniqueIndexes(catalog, schema, tableName, dmd, indexes, rejectedIndexes);
        for (String candidate : indexes.keySet()) {
            if (!rejectedIndexes.contains(candidate)) return indexes.get(candidate);
        }
        return null;
    }

    private void readUniqueIndexes(String catalog, String schema, String tableName, DatabaseMetaData dmd, Map<String, String> indexes, Set<String> rejectedIndexes) {
        ResultSet rs = null;
        try {
            rs = dmd.getIndexInfo(catalog, schema, tableName, true, false);
            while (rs.next()) {
                String colName = rs.getString("COLUMN_NAME");
                String indexName = rs.getString("INDEX_NAME");
                if (indexName == null) {
                    indexName = colName;
                }
                if (indexes.get(indexName) != null) {
                    rejectedIndexes.add(indexName);
                } else {
                    indexes.put(indexName, colName);
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        } finally {
            try {
                rs.close();
            } catch (SQLException e) {
                //do nothing
            }
        }
    }

    private String determinePrimaryKey(String catalog, String schema, String tableName, DatabaseMetaData dmd) {
        String pkn = null;
        ResultSet rs = null;
        try {
            rs = dmd.getPrimaryKeys(catalog, schema, tableName);
            if (!rs.next()) return null;
            pkn = rs.getString("COLUMN_NAME");
            //check whether the primary key is non-composite
            if (rs.next()) return null;
        } catch (SQLException e) {
            throw new RuntimeException(e);
        } finally {
            try {
                rs.close();
            } catch (SQLException e) {
                //do nothing
            }
        }
        return pkn;
    }

    private void readColums(String catalog, String schema, String tableName, DatabaseMetaData dmd, ClassInfo cInfo) throws TableNotFoundException {
        ResultSet rs = null;
        boolean empty = true;
        try {
            rs = dmd.getColumns(catalog, schema, tableName, null);
            while (rs.next()) {
                empty = false;
                String colName = rs.getString("COLUMN_NAME");
                String dbType = rs.getString("TYPE_NAME");
                int javaType = rs.getInt("DATA_TYPE");
                addAttribute(cInfo, colName, dbType, javaType);
            }
        } catch (SQLException ex) {
            throw new RuntimeException(ex);
        } finally {
            try {
                rs.close();
            } catch (SQLException e) {
                // do nothing
            }
        }
        if (empty) {
            throw new TableNotFoundException(tableName);
        }
    }

    private void setAsIdentifier(ClassInfo cInfo, String pkn) {
        for (AttributeInfo ai : cInfo.getAttributes()) {
            if (ai.getColumnName().equals(pkn)) {
                ai.setIdentifier(true);
                break;
            }
        }
    }

    private void addAttribute(ClassInfo cInfo, String colName, String dbType, int javaType) {
        String hibernateType = null;
        try {
            hibernateType = typeMapper.getHibernateType(dbType, javaType);
            AttributeInfo ai = new AttributeInfo();
            ai.setColumnName(colName);
            ai.setFieldName(naming.createPropertyName(colName));
            ai.setHibernateType(hibernateType);
            ai.setCtClass(typeMapper.getCtClass(dbType, javaType));
            cInfo.addAttribute(ai);
        } catch (TypeNotFoundException e) {
            LOGGER.warn("No property generated for attribute " + colName + ": " + e.getMessage());
        }
    }

}
