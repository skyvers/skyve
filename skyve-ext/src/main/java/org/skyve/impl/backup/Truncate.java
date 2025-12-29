package org.skyve.impl.backup;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.Persistence;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

public class Truncate {

    private static final Logger COMMAND_LOGGER = Category.COMMAND.logger();

	public static void truncate(String schemaName, boolean database, boolean content) 
	throws Exception {
		Collection<Table> tables = getTables(schemaName);
		truncate(tables, CORE.getUser().getCustomerName(), database, content);
	}
	
	private static void truncate(Collection<Table> tables, 
									String customerName, 
									boolean database,
									boolean content)
	throws Exception {
		if (database) {
			Persistence persistence = CORE.getPersistence();
			StringBuilder sql = new StringBuilder(128);

			// update foreign keys to null
			for (Table table : tables) { 
				if (table instanceof JoinTable) {
					continue;
				}
				sql.setLength(0);
				sql.append("update ").append(table.persistentIdentifier).append(" set ");
				for (String fieldName : table.fields.keySet()) {
					if (fieldName.toLowerCase().endsWith("_id")) {
						sql.append(fieldName).append(" = null,");
					}
				}
				if (sql.charAt(sql.length() - 1) == ',') {
					sql.setLength(sql.length() - 1); // remove the comma

					BackupUtil.secureSQL(sql, table, customerName);
					if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("unlink table {}", table.persistentIdentifier);
					persistence.newSQL(sql.toString()).noTimeout().execute();
					persistence.commit(false);
					persistence.begin();
				}
			}

			// delete rows from joining tables
			for (Table table : tables) {
				if (table instanceof JoinTable) {
					sql.setLength(0);
					sql.append("delete from ").append(table.persistentIdentifier);
					BackupUtil.secureSQL(sql, table, customerName);
					if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("delete joining table {}", table.persistentIdentifier);
					persistence.newSQL(sql.toString()).noTimeout().execute();
					persistence.commit(false);
					persistence.begin();
				}
			}
			
			// delete rows from joined-extension tables
			for (Table table : tables) {
				if (table instanceof JoinTable) {
					continue;
				}
				if (BackupUtil.hasBizCustomer(table)) {
					continue;
				}
				sql.setLength(0);
				sql.append("delete from ").append(table.persistentIdentifier);
				BackupUtil.secureSQL(sql, table, customerName);
				if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("delete extension table {}", table.persistentIdentifier);
				persistence.newSQL(sql.toString()).noTimeout().execute();
				persistence.commit(false);
				persistence.begin();
			}

			// delete rows from non-joining tables
			for (Table table : tables) {
				if (table instanceof JoinTable) {
					continue;
				}
				if (! BackupUtil.hasBizCustomer(table)) {
					continue;
				}
				sql.setLength(0);
				sql.append("delete from ").append(table.persistentIdentifier);
				BackupUtil.secureSQL(sql, table, customerName);
				if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("delete table {}", table.persistentIdentifier);
				persistence.newSQL(sql.toString()).noTimeout().execute();
				persistence.commit(false);
				persistence.begin();
			}
		}
		
		if (content) {
			try (ContentManager cm = EXT.newContentManager()) {
				cm.truncateIndexing(customerName);
			}
		}
	}

	// table types for getting table database metadata from JDBC
	private static final String[] TABLE_TYPES = new String[] {"TABLE"};
	
	@SuppressWarnings("resource")
	private static Collection<Table> getTables(String schema)
	throws SQLException {
		Collection<Table> result = new ArrayList<>();

		Connection c = ((AbstractHibernatePersistence) AbstractPersistence.get()).getConnection();
		DatabaseMetaData dmd = c.getMetaData();
		String catalog = c.getCatalog();
		// Although not necessarily standard, the following works for H2, MySQL, MSSQL, PostGreSQL
		try (ResultSet tableResultSet = dmd.getTables(catalog, schema, "%", TABLE_TYPES)) {
			while (tableResultSet.next()) {
				String tableName = tableResultSet.getString("TABLE_NAME");
				Table table = new Table(tableName, tableName);
				boolean hasBizIdColumn = false;
				try (ResultSet columnResultSet = dmd.getColumns(catalog, schema, tableName, null)) {
					while (columnResultSet.next()) {
						String columnName = columnResultSet.getString("COLUMN_NAME");
						if (columnName.toLowerCase().endsWith("_id")) {
							table.fields.put(columnName, Table.TEXT);
						}
						else if (columnName.equalsIgnoreCase(Bean.DOCUMENT_ID)) {
							hasBizIdColumn = true;
						}
						// NB Ensure we can detect extension tables
						else if (columnName.equalsIgnoreCase(Bean.CUSTOMER_NAME)) {
							table.fields.put(columnName, Table.TEXT);
						}
					}
				}

				// detect joining tables
				int tableFieldSize = table.fields.size();
				boolean joinTable = ((tableFieldSize == 2) || // unordered collection 
										(tableFieldSize == 3)); // ordered collection
				if (joinTable) {
					// check for owner_id and element_id
					Set<String> columnNames = table.fields.keySet();
					for (String columnName : columnNames) {
						if ((! PersistentBean.OWNER_COLUMN_NAME.equalsIgnoreCase(columnName)) &&
								(! PersistentBean.ELEMENT_COLUMN_NAME.equalsIgnoreCase(columnName)) &&
								(! Bean.ORDINAL_NAME.equalsIgnoreCase(columnName))) {
							joinTable = false;
							break;
						}
					}
					
					if (joinTable) {
						String ownerTableName = null;
						try (ResultSet foreignKeyResultSet = dmd.getImportedKeys(catalog, schema, tableName)) {
							while (foreignKeyResultSet.next()) {
								String foreignKeyColumn = foreignKeyResultSet.getString("FKCOLUMN_NAME");
								if (PersistentBean.OWNER_COLUMN_NAME.equalsIgnoreCase(foreignKeyColumn)) {
									ownerTableName = foreignKeyResultSet.getString("PKTABLE_NAME");
								}
							}
						}

						if (ownerTableName == null) { // is null when the foreign key is not defined
							table = null; // skip this table
							joinTable = false;
						}
						else {
							table = new JoinTable(tableName, tableName, ownerTableName, ownerTableName, (tableFieldSize == 3));
						}
					}
				}
				
				if ((table != null) && 
						(joinTable || hasBizIdColumn)) {
					result.add(table);
				}
			}
			
			// Resolve join tables that have collections on joined extension persistence table strategies
			// The owner Table name should be the ultimate base table (the one that has the bizCustomer column)
			for (Table table : result) {
				if (table instanceof JoinTable joinTable) {
					String ownerPersistentIdentifier = joinTable.ownerPersistentIdentifier;

					// Determine if the owner table has the bizCustomer field or not
					boolean ownerTableHasBizCustomer = false;
					for (Table ownerTable : result) {
						if (ownerPersistentIdentifier.equals(ownerTable.persistentIdentifier)) {
							for (String fieldName : ownerTable.fields.keySet()) {
								if (Bean.CUSTOMER_NAME.equalsIgnoreCase(fieldName)) {
									ownerTableHasBizCustomer = true;
									break;
								}
							}
							break;
						}
					}
					
					// If it does not, look for the target of the FK from the bizId column
					if (! ownerTableHasBizCustomer) {
						// Look for the table name that the bizId FK points to
						try (ResultSet foreignKeyResultSet = dmd.getImportedKeys(catalog, schema, ownerPersistentIdentifier)) {
							while (foreignKeyResultSet.next()) {
								String foreignKeyColumn = foreignKeyResultSet.getString("FKCOLUMN_NAME");
								if (Bean.DOCUMENT_ID.equalsIgnoreCase(foreignKeyColumn)) {
									joinTable.ownerPersistentIdentifier = foreignKeyResultSet.getString("PKTABLE_NAME");
									joinTable.ownerAgnosticIdentifier = joinTable.ownerPersistentIdentifier;
								}
							}
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
			truncate(args[8], true, true);
		}
		finally {
			BackupUtil.finalise();
			
			// This is required to stop the process hanging at the end
			System.exit(0);
		}
	}
}
