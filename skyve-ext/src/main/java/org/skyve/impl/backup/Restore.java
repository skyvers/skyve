package org.skyve.impl.backup;

import java.io.File;
import java.io.FileReader;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.Time;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.Collection;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.elastic.ElasticContentManager;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.Util;
import org.supercsv.io.CsvMapReader;
import org.supercsv.prefs.CsvPreference;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKTReader;

public class Restore {
	public static enum ContentRestoreOption {
		clearOrphanedContentIds,
		saveOrphanedContentIds,
		error
	}
	
	public static void restore(String timestampFolderName,
								boolean useBackupTables,
								ContentRestoreOption conentRestoreOption)
	throws Exception {
		String customerName = CORE.getUser().getCustomerName();
		
		String backupDirectoryPath = UtilImpl.CONTENT_DIRECTORY + 
										"backup_" + customerName + 
										File.separator + timestampFolderName;
		File backupDirectory = new File(backupDirectoryPath);
		if ((! backupDirectory.exists()) || (! backupDirectory.isDirectory())) {
			throw new IllegalArgumentException(backupDirectoryPath + " is not a directory");
		}

		Collection<Table> tables = useBackupTables ? 
										BackupUtil.readTables(new File(backupDirectory, "tables.txt")) :
										BackupUtil.getTables();

		try (Connection connection = EXT.getDataStoreConnection()) {
			connection.setAutoCommit(false);

			// restore normal tables
			restoreData(backupDirectory, tables, connection, false, false, conentRestoreOption);
			// restore extension join tables
			restoreData(backupDirectory, tables, connection, false, true, conentRestoreOption);
			// link foreign keys
			restoreForeignKeys(backupDirectory, tables, connection);
			// restore collection join tables
			restoreData(backupDirectory, tables, connection, true, false, conentRestoreOption);
		}
	}

//	update bizKeys
//	check and remove content not present
//	validate by updating bizLock and rolling back
//	check commit points

	private static void restoreData(File backupDirectory,
										Collection<Table> tables,
										Connection connection,
										boolean joinTables,
										boolean extensionTables,
										ContentRestoreOption contentRestoreOption) 
	throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			for (Table table : tables) {
				if (table instanceof JoinTable) {
					if (! joinTables) {
						continue;
					}
				}
				else {
					if (joinTables) {
						continue;
					}
					if (BackupUtil.hasBizCustomer(table)) {
						if (extensionTables) {
							continue;
						}
					}
					else {
						if (! extensionTables) {
							continue;
						}
					}
				}
				if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info("restore table " + table.name);
				File backupFile = new File(backupDirectory.getAbsolutePath() + File.separator + table.name + ".csv");
				if (! backupFile.exists()) {
					System.err.println("***** File " + backupFile.getAbsolutePath() + File.separator + " does not exist");
					continue;
				}

				long rowCount = 0;

				try (FileReader fr = new FileReader(backupFile)) {
					try (CsvMapReader reader = new CsvMapReader(fr, CsvPreference.STANDARD_PREFERENCE)) {
						String[] headers = reader.getHeader(true);

						StringBuilder sql = new StringBuilder(128);
						sql.append("insert into ").append(table.name).append(" (");
						for (String header : headers) {
							if (joinTables) {
								sql.append(header).append(',');
							}
							else {
								if (! header.endsWith("_id")) {
									sql.append(header).append(',');
								}
							}
						}
						sql.setLength(sql.length() - 1); // remove the last comma
						sql.append(") values (");
						for (String header : headers) {
							if (joinTables) {
								sql.append("?,");
							}
							else {
								if (! header.endsWith("_id")) {
									sql.append("?,");
								}
							}
						}
						sql.setLength(sql.length() - 1); // remove the last comma
						sql.append(')');

						Map<String, String> values = null;
						try (PreparedStatement statement = connection.prepareStatement(sql.toString())) {
							while ((values = reader.read(headers)) != null) {
								statement.clearParameters();

								int index = 1;
								for (String header : headers) {
									if ((! joinTables) && header.endsWith("_id")) {
										continue;
									}
									String stringValue = values.get(header);
									if ((stringValue == null) || (stringValue.length() == 0)) {
										statement.setObject(index++, null);
										continue;
									}

									AttributeType attributeType = table.fields.get(header);

									// replace any 2 CR or LF combinations in the string with 1
									// Super CSV place 2 ox0A into the string when it comes across a '\n'
									// in a quoted string field value.
									stringValue = stringValue.replaceAll("[\\n\\r]{2}", "\n");

									// foreign keys
									if (header.endsWith("_id")) {
										statement.setString(index++, stringValue);
									}
									else if (AttributeType.colour.equals(attributeType) ||
												AttributeType.memo.equals(attributeType) ||
												AttributeType.markup.equals(attributeType) ||
												AttributeType.text.equals(attributeType) ||
												AttributeType.enumeration.equals(attributeType) ||
												AttributeType.id.equals(attributeType)) {
										statement.setString(index++, stringValue);
									}
									else if (AttributeType.geometry.equals(attributeType)) {
										Geometry geometry = new WKTReader().read(stringValue);
										SkyveDialect dialect = AbstractHibernatePersistence.getDialect();
										int geometrySqlType = dialect.getGeometrySqlType();
										if (geometrySqlType == Types.ARRAY) {
											statement.setBytes(index++, (byte[]) dialect.convertToPersistedValue(geometry));
										}
										else {
											statement.setObject(index++, 
																	dialect.convertToPersistedValue(geometry), 
																	geometrySqlType);
										}
									}
									else if (AttributeType.bool.equals(attributeType)) {
										statement.setBoolean(index++, Boolean.parseBoolean(stringValue));
									}
									else if (AttributeType.date.equals(attributeType)) {
										statement.setDate(index++, new Date(Long.parseLong(stringValue)), BackupUtil.GMT);
									}
									else if (AttributeType.time.equals(attributeType)) {
										statement.setTime(index++, new Time(Long.parseLong(stringValue)), BackupUtil.GMT);
									}
									else if (AttributeType.dateTime.equals(attributeType) ||
												AttributeType.timestamp.equals(attributeType)) {
										statement.setTimestamp(index++, new Timestamp(Long.parseLong(stringValue)), BackupUtil.GMT);
									}
									else if (AttributeType.decimal2.equals(attributeType) ||
												AttributeType.decimal5.equals(attributeType) ||
												AttributeType.decimal10.equals(attributeType)) {
										statement.setBigDecimal(index++, new BigDecimal(stringValue));
									}
									else if (AttributeType.integer.equals(attributeType)) {
										statement.setInt(index++, Integer.parseInt(stringValue));
									}
									else if (AttributeType.longInteger.equals(attributeType)) {
										statement.setLong(index++, Long.parseLong(stringValue));
									}
									else if (AttributeType.content.equals(attributeType)) {
										StringBuilder contentPath = new StringBuilder(128);
										contentPath.append(backupDirectory.getAbsolutePath()).append('/');
										AbstractContentManager.appendBalancedFolderPathFromContentId(stringValue, contentPath, false);

										AttachmentContent content = ElasticContentManager.getFromFileSystem(contentPath, stringValue);
										if (content == null) {
											String message = "Could not find file associated with " + stringValue;
											if (ContentRestoreOption.error.equals(contentRestoreOption)) {
												Util.LOGGER.severe(message);
												throw new DomainException(message);
											}
											else if (ContentRestoreOption.clearOrphanedContentIds.equals(contentRestoreOption)) {
												Util.LOGGER.info(message + " : Setting content to null");
												statement.setString(index++, null);
											}
											else {
												Util.LOGGER.info(message + " : Setting content ID regardless");
												statement.setString(index++, stringValue);
											}
										}
										else {
											cm.put(content);
											statement.setString(index++, content.getContentId());
										}
									}
									else {
										throw new IllegalStateException("No value set for " + header);
									}
								} // for (each header)

								statement.executeUpdate();
								rowCount++;
							} // while (each CSV line)

							connection.commit();
						}
						catch (Throwable t) {
							Util.LOGGER.severe(t.getLocalizedMessage());
							Util.LOGGER.severe("AT LINE " + rowCount + " OF " + backupFile.getAbsolutePath());
							Util.LOGGER.severe("CAUSED BY:- " + sql.toString());
							
							StringBuilder sb = new StringBuilder(512);
							sb.append("VALUES  :- ");
							if (values == null) {
								sb.append("NONE");
							}
							else {
								for (String header : values.keySet()) {
									sb.append(header).append('=').append(values.get(header)).append(',');
								}
								sb.setLength(sb.length() - 1); // remove last comma
							}
							Util.LOGGER.severe(sb.toString());
							
							throw t;
						}
					}
				}
				if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info("restored table " + table.name + " with " + rowCount + " rows.");
			} // for (each table)
		}
	}

	private static void restoreForeignKeys(File backupDirectory, 
											Collection<Table> tables, 
											Connection connection)
	throws Exception {
		for (Table table : tables) {
			if (table instanceof JoinTable) {
				continue;
			}
			if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info("restore foreign keys for table " + table.name);
			File backupFile = new File(backupDirectory.getAbsolutePath() + File.separator + table.name + ".csv");
			if (! backupFile.exists()) {
				System.err.println("***** File " + backupFile.getAbsolutePath() + File.separator + " does not exist");
				continue;
			}
			
			try (FileReader fr = new FileReader(backupFile)) {
				try (CsvMapReader reader = new CsvMapReader(fr, CsvPreference.STANDARD_PREFERENCE)) {
					String[] headers = reader.getHeader(true);

					StringBuilder sql = new StringBuilder(128);
					sql.append("update ").append(table.name);
					boolean foundAForeignKey = false;
					for (String header : headers) {
						if (header.endsWith("_id")) {
							if (! foundAForeignKey) {
								sql.append(" set ");
							}
							else {
								sql.append(", ");
							}
							sql.append(header).append(" = ?");
							foundAForeignKey = true;
						}
					}

					if (foundAForeignKey) {
						sql.append(" where bizId = ?");
						
						try (PreparedStatement statement = connection.prepareStatement(sql.toString())) {
							Map<String, String> values = null;
							while ((values = reader.read(headers)) != null) {
								statement.clearParameters();

								int i = 1;
								for (String header : headers) {
									if (header.endsWith("_id")) {
										final String stringValue = values.get(header);
										if ((stringValue == null) || (stringValue.length() == 0)) {
											statement.setObject(i, null);
											i++;
										}
										else {
											statement.setString(i, stringValue);
											i++;
										}
									}
								} // for (each header)

								// set the ID for the where clause
								statement.setString(i, values.get(Bean.DOCUMENT_ID));
								statement.executeUpdate();
							} // while (each CSV line)

							connection.commit();
						}
					}
				}
			}
		} // for (each table)
	}

	public static void main(String[] args) throws Exception {
		if (args.length != 9) {
			System.err.println("args are <customerName> <content directory> <content file storage?> <DB dialect> <DB driver> <DB URL> <DB username> <DB password> <backup timestamp>");
			System.exit(1);
		}

		BackupUtil.initialise(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
		try {
			restore(args[8], true, ContentRestoreOption.error);
		}
		finally {
			BackupUtil.finalise();
			
			// This is required to stop the process hanging at the end
			System.exit(0);
		}
	}
}
