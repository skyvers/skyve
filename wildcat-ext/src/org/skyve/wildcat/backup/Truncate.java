package org.skyve.wildcat.backup;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.wildcat.content.ContentManager;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.wildcat.util.UtilImpl;

public class Truncate {
	public static void truncate(String schemaName) 
	throws Exception {
		Collection<Table> tables = getTables(schemaName);
		truncate(tables, CORE.getUser().getCustomerName());
	}
	
	private static void truncate(Collection<Table> tables, String customerName) throws Exception {
		StringBuilder sql = new StringBuilder(128);

		AbstractPersistence persistence = AbstractPersistence.get();
		try {
			persistence.begin();

			// update foreign keys to null
			for (Table table : tables) { 
				if (table instanceof JoinTable) {
					continue;
				}
				sql.setLength(0);
				sql.append("update ").append(table.name).append(" set ");
				for (String fieldName : table.fields.keySet()) {
					if (fieldName.toLowerCase().endsWith("_id")) {
						sql.append(fieldName).append(" = null,");
					}
				}
				if (sql.charAt(sql.length() - 1) == ',') {
					sql.setLength(sql.length() - 1); // remove the comma

					BackupUtil.secureSQL(sql, table, customerName);
					persistence.newSQL(sql.toString()).execute();
				}
			}

			// delete rows from joining tables
			for (Table table : tables) {
				if (table instanceof JoinTable) {
					sql.setLength(0);
					sql.append("delete from ").append(table.name);
					BackupUtil.secureSQL(sql, table, customerName);
					UtilImpl.LOGGER.info("delete table " + table.name);
					persistence.newSQL(sql.toString()).execute();
				}
			}
			
			// delete rows from non-joining tables
			for (Table table : tables) {
				if (table instanceof JoinTable) {
					continue;
				}
				sql.setLength(0);
				sql.append("delete from ").append(table.name);
				BackupUtil.secureSQL(sql, table, customerName);
				UtilImpl.LOGGER.info("delete table " + table.name);
				persistence.newSQL(sql.toString()).execute();
			}
		}
		finally {
			persistence.commit(false);
		}
		
		try (ContentManager cm = EXT.newContentManager()) {
			cm.truncate(customerName);
		}
	}

	private static Collection<Table> getTables(String schema)
	throws SQLException {
		Collection<Table> result = new ArrayList<>();

		try (Connection c = ((AbstractHibernatePersistence) AbstractPersistence.get()).getConnection()) {
			DatabaseMetaData dmd = c.getMetaData();
			String catalog = c.getCatalog();
			try (ResultSet tableResultSet = dmd.getTables(catalog, schema, "%", null)) {
				while (tableResultSet.next()) {
					String tableName = tableResultSet.getString("TABLE_NAME");
					String tableType = tableResultSet.getString("TABLE_TYPE");
					if ("TABLE".equalsIgnoreCase(tableType)) {
						Table table = new Table(tableName);
						boolean hasBizIdColumn = false;
						try (ResultSet columnResultSet = dmd.getColumns(catalog, schema, tableName, null)) {
							while (columnResultSet.next()) {
								String columnName = columnResultSet.getString("COLUMN_NAME");
								if (columnName.toLowerCase().endsWith("_id")) {
									table.fields.put(columnName, AttributeType.text);
								}
								if (columnName.equalsIgnoreCase(Bean.DOCUMENT_ID)) {
									hasBizIdColumn = true;
								}
							}
						}

						// detect joining tables
						boolean joinTable = (table.fields.size() == 2);
						if (joinTable) {
							// check for owner_id and element_id
							Set<String> columnNames = table.fields.keySet();
							for (String columnName : columnNames) {
								if ((! "owner_id".equalsIgnoreCase(columnName)) &&
										(! "element_id".equalsIgnoreCase(columnName))) {
									joinTable = false;
									break;
								}
							}
							
							if (joinTable) {
								String ownerTableName = null;
								try (ResultSet foreignKeyResultSet = dmd.getImportedKeys(catalog, schema, tableName)) {
									while (foreignKeyResultSet.next()) {
										String foreignKeyColumn = foreignKeyResultSet.getString("FKCOLUMN_NAME");
										if ("owner_id".equalsIgnoreCase(foreignKeyColumn)) {
											ownerTableName = foreignKeyResultSet.getString("PKTABLE_NAME");
										}
									}
								}

								if (ownerTableName == null) { // is null when the foreign key is not defined
									table = null; // skip this table
									joinTable = false;
								}
								else {
									table = new JoinTable(tableName, ownerTableName);
								}
							}
						}
						
						if ((table != null) && 
								(joinTable || hasBizIdColumn)) {
							result.add(table);
						}
					}
				}
			}
		}

		return result;
	}
	
	public static void main(String[] args) throws Exception {
		if (args.length != 9) {
			System.err.println("args are <customerName> <content directory> <content file storage?> <DB dialect> <DB driver> <DB URL> <DB username> <DB password> <DB schema>");
			System.exit(1);
		}
		BackupUtil.initialise(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
		try {
			truncate(args[8]);
		}
		finally {
			BackupUtil.finalise();
			
			// This is required to stop the process hanging at the end
			System.exit(0);
		}
	}
}
