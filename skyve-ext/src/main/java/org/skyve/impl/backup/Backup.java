package org.skyve.impl.backup;

import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.io.FilenameUtils;
import org.hibernate.engine.spi.SessionImplementor;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.ThreadSafeFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.util.Util;
import org.supercsv.io.CsvMapWriter;
import org.supercsv.prefs.CsvPreference;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKTWriter;

/**
 * Tables and the content repository files are backed up by this.
 * The fields are added to the tables taking into account that 
 * there may be multiple documents mapped onto the same table.
 * But we only want one copy of each table.
 * The customer data is separated out in the data base.
 * 
 * Each content file contains an associated named properties file 
 * that contains all the information needed to construct the path
 * of the content node - ie module name and document name are not known to the table.
 */
public class Backup {
	private Backup() {
		// nothing to see here
	}

	public static File backup() throws Exception {
		return backup(BackupUtil.getTables());
	}

	public static File backupDocuments(Collection<Document> documents) throws Exception {
		return backup(BackupUtil.getTables(documents));
	}
	
	public static File backup(Collection<Table> tables) throws Exception {
		String customerName = CORE.getUser().getCustomerName();
		
		String backupDir = String.format("%sbackup_%s%s%s%s",
											UtilImpl.CONTENT_DIRECTORY, 
											customerName, 
											File.separator, 
											ThreadSafeFactory.getDateFormat("yyyyMMddHHmmss").format(new java.util.Date()),
											File.separator);
		File directory = new File(backupDir);
		directory.mkdirs();
		UtilImpl.LOGGER.info("Backup to " + directory.getAbsolutePath());

		BackupUtil.writeTables(tables, new File(backupDir, "tables.txt"));

		AbstractPersistence p = AbstractPersistence.get();
		List<String> drops = new ArrayList<>();
		List<String> creates = new ArrayList<>();
		p.generateDDL(drops, creates, null);
		BackupUtil.writeScript(drops, new File(backupDir, "drop.sql"));
		BackupUtil.writeScript(creates, new File(backupDir, "create.sql"));

		try (FileWriter problemsTxt = new FileWriter(new File(backupDir, "problems.txt"))) {
			try (BufferedWriter problems = new BufferedWriter(problemsTxt)) {
				try (Connection connection = EXT.getDataStoreConnection()) {
					connection.setAutoCommit(false);
		
					try (ContentManager cm = EXT.newContentManager()) {
						for (Table table : tables) {
							StringBuilder sql = new StringBuilder(128);
							try (Statement statement = connection.createStatement()) {
								sql.append("select * from ").append(table.name);
								BackupUtil.secureSQL(sql, table, customerName);
								statement.execute(sql.toString());
								try (ResultSet resultSet = statement.getResultSet()) {
									if (UtilImpl.COMMAND_TRACE) UtilImpl.LOGGER.info("Backup " + table.name);
									try (FileWriter csv = new FileWriter(backupDir + File.separator + table.name + ".csv",
																			false)) {
										try (CsvMapWriter writer = new CsvMapWriter(csv, CsvPreference.STANDARD_PREFERENCE)) {
											Map<String, Object> values = new TreeMap<>();
											String[] headers = new String[table.fields.size()];
											headers = table.fields.keySet().toArray(headers);
			
											writer.writeHeader(headers);
			
											while (resultSet.next()) {
												values.clear();
			
												for (String name : table.fields.keySet()) {
													AttributeType attributeType = table.fields.get(name);
													Object value = null;
			
													if (AttributeType.association.equals(attributeType) ||
															AttributeType.colour.equals(attributeType) ||
															AttributeType.memo.equals(attributeType) ||
															AttributeType.markup.equals(attributeType) ||
															AttributeType.text.equals(attributeType) ||
															AttributeType.enumeration.equals(attributeType) ||
															AttributeType.id.equals(attributeType)) {
														value = resultSet.getString(name);
														if (resultSet.wasNull()) {
															value = "";
														}
														if ("".equals(value)) {
															// bizId is mandatory
															if (name.equalsIgnoreCase(Bean.DOCUMENT_ID)) {
																throw new IllegalStateException(table.name + " is missing a " + Bean.DOCUMENT_ID + " value.");
															}
															// bizLock is mandatory
															if (name.equalsIgnoreCase(PersistentBean.LOCK_NAME)) {
																throw new IllegalStateException(table.name + " with " + 
																									Bean.DOCUMENT_ID + " = " + values.get(Bean.DOCUMENT_ID) +
																									" is missing a " + PersistentBean.LOCK_NAME + " value.");
															}
															// bizKey is mandatory
															if (name.equalsIgnoreCase(Bean.BIZ_KEY)) {
																throw new IllegalStateException(table.name + " with " + 
																									Bean.DOCUMENT_ID + " = " + values.get(Bean.DOCUMENT_ID) +
																									" is missing a " + Bean.BIZ_KEY + " value.");
															}
															// bizCustomer is mandatory
															if (name.equalsIgnoreCase(Bean.CUSTOMER_NAME)) {
																throw new IllegalStateException(table.name + " with " + 
																									Bean.DOCUMENT_ID + " = " + values.get(Bean.DOCUMENT_ID) +
																									" is missing a " + Bean.CUSTOMER_NAME + " value.");
															}
															// bizUserId is mandatory
															if (name.equalsIgnoreCase(Bean.USER_ID)) {
																throw new IllegalStateException(table.name + " with " + 
																									Bean.DOCUMENT_ID + " = " + values.get(Bean.DOCUMENT_ID) +
																									" is missing a " + Bean.USER_ID + " value.");
															}
														}
													}
													else if (AttributeType.geometry.equals(attributeType)) {
														@SuppressWarnings("resource")
														SessionImplementor sessionImpl = (SessionImplementor) ((AbstractHibernatePersistence) p).getSession();
														Geometry geometry = AbstractHibernatePersistence.getDialect().getGeometryType().nullSafeGet(resultSet, name, sessionImpl);
														if (geometry == null) {
															value = "";
														}
														else {
															value = new WKTWriter().write(geometry);
														}
													}
													else if (AttributeType.bool.equals(attributeType)) {
														boolean booleanValue = resultSet.getBoolean(name);
														if (resultSet.wasNull()) {
															value = "";
														}
														else {
															value = Boolean.valueOf(booleanValue);
														}
													}
													else if (AttributeType.date.equals(attributeType)) {
														Date date = resultSet.getDate(name, BackupUtil.GMT);
														if (resultSet.wasNull()) {
															value = "";
														}
														else {
															value = new Long(date.getTime());
														}
													}
													else if (AttributeType.time.equals(attributeType)) {
														Time time = resultSet.getTime(name, BackupUtil.GMT);
														if (resultSet.wasNull()) {
															value = "";
														}
														else {
															value = new Long(time.getTime());
														}
													}
													else if (AttributeType.dateTime.equals(attributeType) ||
																AttributeType.timestamp.equals(attributeType)) {
														Timestamp timestamp = resultSet.getTimestamp(name, BackupUtil.GMT);
														if (resultSet.wasNull()) {
															value = "";
														}
														else {
															value = new Long(timestamp.getTime());
														}
													}
													else if (AttributeType.decimal2.equals(attributeType) ||
																AttributeType.decimal5.equals(attributeType) ||
																AttributeType.decimal10.equals(attributeType)) {
														BigDecimal bigDecimal = resultSet.getBigDecimal(name);
														if (resultSet.wasNull()) {
															value = "";
														}
														else {
															value = bigDecimal;
														}
													}
													else if (AttributeType.integer.equals(attributeType)) {
														int intValue = resultSet.getInt(name);
														if (resultSet.wasNull()) {
															value = "";
														}
														else {
															value = new Integer(intValue);
														}
														// bizVersion is mandatory
														if ("".equals(value) && 
																name.equalsIgnoreCase(PersistentBean.VERSION_NAME)) {
															throw new IllegalStateException(table.name + " with " + 
																								Bean.DOCUMENT_ID + " = " + values.get(Bean.DOCUMENT_ID) +
																								" is missing a " + PersistentBean.VERSION_NAME + " value.");
														}
		
													}
													else if (AttributeType.longInteger.equals(attributeType)) {
														long longValue = resultSet.getLong(name);
														if (resultSet.wasNull()) {
															value = "";
														}
														else {
															value = new Long(longValue);
														}
													}
													else if (AttributeType.content.equals(attributeType)) {
														String stringValue = resultSet.getString(name);
														if (resultSet.wasNull()) {
															value = "";
														}
														else {
															value = stringValue;
															AttachmentContent content = null;
															try {
																content = cm.get(stringValue);
																if (content == null) {
																	problems.write(String.format("Table [%s] with [%s] = %s is missing content for attribute [%s] = %s",
																									table.name,
																									Bean.DOCUMENT_ID,
																									values.get(Bean.DOCUMENT_ID),
																									name,
																									stringValue));
																	// See if the content file exists 
																	final File contentDirectory = Paths.get(UtilImpl.CONTENT_DIRECTORY, AbstractContentManager.FILE_STORE_NAME).toFile();
																	final StringBuilder contentAbsolutePath = new StringBuilder(contentDirectory.getAbsolutePath()).append(File.separator);
																	AbstractContentManager.appendBalancedFolderPathFromContentId(stringValue, contentAbsolutePath);
																	final File contentFile = Paths.get(contentAbsolutePath.toString(), stringValue).toFile();
																	if (contentFile.exists()) {
																		problems.write(" but the matching file was found for this missing content at ");
																		problems.write(contentFile.getAbsolutePath());
																	}
																	problems.newLine();
																}
																else {
																	try (InputStream cis = content.getContentStream()) {
																		StringBuilder contentPath = new StringBuilder(256);
																		contentPath.append(directory.getAbsolutePath()).append('/');
																		contentPath.append(content.getBizModule()).append('/');
																		contentPath.append(content.getBizDocument()).append('/');
																		AbstractContentManager.appendBalancedFolderPathFromContentId(stringValue, contentPath);
																		contentPath.append(stringValue);
																		File contentDirectory = new File(contentPath.toString());
																		if (! contentDirectory.exists()) {
																			contentDirectory.mkdirs();
																		}
																		String fileName = content.getFileName();
																		if (fileName == null) {
																			fileName = "attachment." + content.getMimeType().getStandardFileSuffix();
																		}
																		else {
																			// remove the path
																			fileName = FilenameUtils.getName(fileName);
																			// remove any invalid chars on all OSs (restricted by windows)
																			fileName = fileName.replaceAll("[\u0001-\u001f<>:\"/\\\\|?*\u007f]+", "").trim();
																		}
																		try (FileOutputStream cos = new FileOutputStream(contentDirectory.getAbsolutePath() +
																															File.separator + fileName)) {
																			try (BufferedOutputStream bos = new BufferedOutputStream(cos)) {
																				byte[] bytes = new byte[1024]; // 1K
																				int bytesRead = 0;
																				while ((bytesRead = cis.read(bytes)) > 0) {
																					bos.write(bytes, 0, bytesRead);
																				}
																				bos.flush();
																			}
																		}
																	}
																}
															}
															catch (Exception e) {
																if (e instanceof FileNotFoundException) {
																	problems.write(String.format("Table [%s] with [%s] = %s is missing a file in the content store for attribute [%s] = %s",
																									table.name,
																									Bean.DOCUMENT_ID,
																									values.get(Bean.DOCUMENT_ID),
																									name,
																									stringValue));
																	problems.newLine();
																}
																else {
																	throw e;
																}
															}
														}
													}
			
													values.put(name, value);
												}
			
												writer.write(values, headers);
											}
										}
									}
								}
							}
							catch (SQLException e) {
								Util.LOGGER.severe(sql.toString());
								throw e;
							}
						}
						connection.commit();
					}
				}
			}
		}
		
		return directory;
	}
	
	public static void main(String[] args) throws Exception {
		if (args.length != 8) {
			System.err.println("args are <customerName> <content directory> <content file storage?> <DB dialect> <DB driver> <DB URL> <DB username> <DB password>");
			System.exit(1);
		}
		BackupUtil.initialise(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
		try {
			backup();
		}
		finally {
			BackupUtil.finalise();
			
			// This is required to stop the process hanging at the end
			System.exit(0);
		}
	}
}
