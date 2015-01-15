package org.skyve.wildcat.tools.backup;

import java.io.File;
import java.io.FileWriter;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.persistence.hibernate.AbstractHibernatePersistence;

class ForeignKey {
	private String table;
	private String field;
	private String fkName;
	
	public ForeignKey(String table, String field) {
		super();
		this.table = table;
		this.field = field;
	}

	public String getTable() {
		return table;
	}

	public String getField() {
		return field;
	}

	public String getFkName() {
		return fkName;
	}
	
	public void setFkName(String fkName) {
		this.fkName= fkName;
	}
}

public class Validate {
	/**
	 * Tables and the content repository files are Validated up by this.
	 */
	private static void validate(Collection<Table> tables, String customerName)
	throws Exception {

		final String LINE_SEP = System.getProperty("line.separator");
		final String BACKUP_DIR_PREFIX = "./alter_";

		File directory = new File(BACKUP_DIR_PREFIX + customerName + File.separator);
		directory.mkdirs();

		AbstractHibernatePersistence persistence = (AbstractHibernatePersistence) AbstractPersistence.get();
		try {
			StringBuilder fullDiff = new StringBuilder();
			StringBuilder alter = new StringBuilder();

			try (Connection connection = persistence.getConnection()) {
				DatabaseMetaData dbmd = connection.getMetaData();
				List<String> metaDataTableNames = new ArrayList<>();
	
				for (Table table : tables) {
					metaDataTableNames.add(table.name);
					List<ForeignKey> fks = new ArrayList<>();
					List<String> columns = new ArrayList<>();
					Set<String> tableFields = table.fields.keySet(); 
					try (ResultSet rs = dbmd.getColumns(null, null, table.name, null)) {
						while (rs.next()) {
							String fieldName = rs.getString("COLUMN_NAME");
							
							if(!tableFields.contains(fieldName)) {
								
								fullDiff.append("Table - ").append(table.name).append(" field - ").append(fieldName).append(LINE_SEP);
								if(fieldName.endsWith("_id")) {
									fks.add(new ForeignKey(table.name, fieldName));
								}
								if (!fieldName.startsWith("biz")) {
									columns.add(fieldName);
								}
							}
						}
					}
					
					if (! fks.isEmpty()) {
						for (ForeignKey fk : fks) {
							try (ResultSet rs = dbmd.getImportedKeys(null, null, fk.getTable())) {
								while(rs.next()) {
									if(rs.getString("FKCOLUMN_NAME").equals(fk.getField())) {
										fk.setFkName(rs.getString("FK_NAME"));
										break;
									}
								}
							}
						}
		
						for (ForeignKey fk : fks) {
							alter.append("ALTER TABLE ").append(fk.getTable()).append(" DROP CONSTRAINT ").append(fk.getFkName()).append(";").append(LINE_SEP);
						}				
					}
					
					if (! columns.isEmpty()) {
						for (String column : columns) {
							alter.append("ALTER TABLE ").append(table.name).append(" DROP COLUMN ").append(column).append(";").append(LINE_SEP);
						}
					}
				}
			}
			
			try (FileWriter fw = new FileWriter(BACKUP_DIR_PREFIX + customerName + 
														File.separator + "DropColumnAndTable.sql",
													false)) {
				fw.write(alter.toString().toCharArray());
			}
			
			try (FileWriter fw = new FileWriter(BACKUP_DIR_PREFIX + customerName + 
														File.separator + "FullDifference.txt", 
													false)) {
				fw.write(fullDiff.toString().toCharArray());
			}
		}
		finally {
			persistence.commit(true);
		}
	}

	public static void main(String[] args) throws Exception {
		if (args.length != 7) {
			System.err.println("args are <customerName> <content directory> <DB dialect> <DB driver> <DB URL> <DB username> <DB password>");
			for(int i = 0; i < args.length; ++i) {
				System.err.println("parameter " + i + " = >" + args[i] + "<");
			}
			System.exit(1);
		}
		BackupUtil.initialize(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
		Collection<Table> tables = BackupUtil.getTables();
		validate(tables, args[0]);
	}
}
