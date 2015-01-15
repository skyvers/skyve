package org.skyve.wildcat.tools.backup;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.Date;
import java.util.Map;

import javax.jcr.Session;

import org.hibernate.usertype.UserType;
import org.hibernatespatial.SpatialDialect;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.wildcat.content.ContentUtil;
import org.skyve.wildcat.content.StreamContent;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.persistence.hibernate.HibernatePersistence;
import org.skyve.wildcat.util.UtilImpl;
import org.supercsv.io.CsvMapReader;
import org.supercsv.prefs.CsvPreference;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKTReader;

public class Restore {
	private static void restore(String customerName,
									String contentDirectory,
									String databaseDialect,
									String databaseJdbcDriver,
									String databaseConnectionUrl,
									String databaseUsername,
									String databasePassword) 
	throws Exception {
		String backupDirectoryPath = "./backup_" + customerName;
		File backupDirectory = new File(backupDirectoryPath);
		if ((! backupDirectory.exists()) || (! backupDirectory.isDirectory())) {
			throw new IllegalArgumentException(backupDirectoryPath + " is not a directory");
		}

		BackupUtil.initialize(customerName, 
								contentDirectory, 
								databaseDialect,
								databaseJdbcDriver, 
								databaseConnectionUrl, 
								databaseUsername, 
								databasePassword);
		Collection<Table> tables = BackupUtil.getTables();

		restoreData(customerName, backupDirectory, tables, false);
		restoreForeignKeys(backupDirectory, tables);
		restoreData(customerName, backupDirectory, tables, true);
	}

//	update bizKeys
//	check and remove content not present
//	validate by updating bizLock and rolling back
//	check commit points

	private static void restoreData(String customerName,
										File backupDirectory,
										Collection<Table> tables,
										boolean joinTables) 
	throws Exception {
		UserType geometryUserType = null; // this is only created when we come across a geometry

		HibernatePersistence persistence = (HibernatePersistence) AbstractPersistence.get();
		try {
			persistence.begin();
			Session jcrSession = null;
			try {
				// Don't close this connection
				@SuppressWarnings("resource")
				Connection connection = persistence.getConnection();
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
					}
					UtilImpl.LOGGER.info("restore table " + table.name);
					File backupFile = new File(backupDirectory.getAbsolutePath() + File.separator + table.name + ".csv");
					if (! backupFile.exists()) {
						System.err.println("***** File " + backupFile.getAbsolutePath() + File.separator + " does not exist");
						continue;
					}
					try (FileReader fr = new FileReader(backupFile)) {
						CsvMapReader reader = new CsvMapReader(fr, CsvPreference.STANDARD_PREFERENCE);
						try {
							String[] headers = reader.getCSVHeader(true);
	
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
	
							try (PreparedStatement statement = connection.prepareStatement(sql.toString())) {
								Map<String, String> values = null;
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
													AttributeType.enumeration.equals(attributeType)) {
											statement.setString(index++, stringValue);
										}
										else if (attributeType == AttributeType.geometry) {
											Geometry geometry = new WKTReader().read(stringValue);
											if (geometryUserType == null) {
												SpatialDialect dialect = (SpatialDialect) Class.forName(UtilImpl.DIALECT).newInstance();
												geometryUserType = dialect.getGeometryUserType();
											}
											geometryUserType.nullSafeSet(statement, geometry, index++);
										}
										else if (attributeType == AttributeType.bool) {
											statement.setBoolean(index++, Boolean.parseBoolean(stringValue));
										}
										else if ((attributeType == AttributeType.date) || 
													(attributeType == AttributeType.dateTime) ||
													(attributeType == AttributeType.time) ||
													(attributeType == AttributeType.timestamp)) {
											statement.setTimestamp(index++, new Timestamp(Long.parseLong(stringValue)));
										}
										else if ((attributeType == AttributeType.decimal2) ||
													(attributeType == AttributeType.decimal5) ||
													(attributeType == AttributeType.decimal10)) {
											statement.setBigDecimal(index++, new BigDecimal(stringValue));
										}
										else if (attributeType == AttributeType.integer) {
											statement.setInt(index++, Integer.parseInt(stringValue));
										}
										else if (attributeType == AttributeType.longInteger) {
											statement.setLong(index++, Long.parseLong(stringValue));
										}
										else if (attributeType == AttributeType.content) {
											// check the relative content paths for the content file
											File contentFile = null;
											String moduleName = null;
											String documentName = null;
											final String fileName = stringValue;
											for (String relativeContentPath : table.relativeContentPaths) {
												File candidateDirectory = new File(backupDirectory.getAbsolutePath() + File.separator +
																					relativeContentPath + File.separator + 
																					stringValue.substring(0, 2) + File.separator + 
																					stringValue.substring(2, 4) + File.separator +
																					stringValue.substring(4, 6));
												File[] files = candidateDirectory.listFiles(new FilenameFilter() {
													@Override
													public boolean accept(File dir, String name) {
														return (name.contains(fileName));
													}
												});
												if ((files != null) && // directory exists
														(files.length == 1)) { // has the file in it
													contentFile = files[0];
													int separatorIndex = relativeContentPath.indexOf('/');
													moduleName = relativeContentPath.substring(0, separatorIndex);
													documentName = relativeContentPath.substring(separatorIndex + 1);
													break;
												}
											}
											if (contentFile == null) {
												System.err.println("Could not find file associated with " + stringValue);
												statement.setString(index++, null);
											}
											else {
												if (jcrSession == null) {
													jcrSession = ContentUtil.getFullSession(customerName);
												}
	
												String dataGroupId = values.get(Bean.DATA_GROUP_ID);
												if (dataGroupId.isEmpty()) {
													dataGroupId = null;
												}
												// TODO restore content versions
												StreamContent content = new StreamContent(values.get(Bean.CUSTOMER_NAME), 
																							moduleName,
																							documentName, 
																							dataGroupId, 
																							values.get(Bean.USER_ID), 
																							values.get(Bean.DOCUMENT_ID), 
																							header);
												try (BufferedInputStream stream = new BufferedInputStream(new FileInputStream(contentFile))) {
													content.setStream(stream);
													content.setLastModified(new Date());
													content.setMimeType(MimeType.fromFileName(contentFile.getName()));
													content.setUuid(fileName);
													ContentUtil.put(jcrSession, content, true);
												}
	
												statement.setString(index++, content.getUuid());
											}
										}
										else {
											throw new IllegalStateException("No value set for " + header);
										}
									} // for (each header)
	
									statement.executeUpdate();
								} // while (each CSV line)
	
								connection.commit();
							}
						}
						finally {
							reader.close();
						}
					}
				} // for (each table)
			}
			finally {
				if (jcrSession != null) {
					jcrSession.logout();
				}
			}
		}
		finally {
			persistence.commit(true);
		}
	}

	private static void restoreForeignKeys(File backupDirectory, Collection<Table> tables) throws Exception {
		HibernatePersistence persistence = (HibernatePersistence) AbstractPersistence.get();
		try {
			persistence.begin();
			// Don't close this connection
			@SuppressWarnings("resource")
			Connection connection = persistence.getConnection();
			for (Table table : tables) {
				if (table instanceof JoinTable) {
					continue;
				}
				UtilImpl.LOGGER.info("restore foreign keys for table " + table.name);
				File backupFile = new File(backupDirectory.getAbsolutePath() + File.separator + table.name + ".csv");
				if (! backupFile.exists()) {
					System.err.println("***** File " + backupFile.getAbsolutePath() + File.separator + " does not exist");
					continue;
				}
				
				try (FileReader fr = new FileReader(backupFile)) {
					CsvMapReader reader = new CsvMapReader(fr, CsvPreference.STANDARD_PREFERENCE);
					try {
						String[] headers = reader.getCSVHeader(true);
	
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
					finally {
						reader.close();
					}
				}
			} // for (each table)
		}
		finally {
			persistence.commit(true);
		}
	}

	public static void main(String[] args) throws Exception {
		if (args.length != 7) {
			System.err.println("args are <customerName> <content directory> <DB dialect> <DB driver> <DB URL> <DB username> <DB password>");
			System.exit(1);
		}
		restore(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
	}
}
