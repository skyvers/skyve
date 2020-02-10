package org.skyve.impl.backup;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.OutputStreamWriter;
import java.math.BigDecimal;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.hibernate.engine.spi.SessionImplementor;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.WKTWriter;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.elastic.ElasticContentManager;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.ThreadSafeFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.CancellableJob;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.FileUtil;
import org.skyve.util.PushMessage;
import org.skyve.util.Util;
import org.supercsv.io.CsvMapWriter;
import org.supercsv.prefs.CsvPreference;

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
public class BackupJob extends CancellableJob {
	private static final long serialVersionUID = 1590777761018078475L;

	private File backupZip;
	
	public File getBackupZip() {
		return backupZip;
	}

	@Override
	public void execute() throws Exception {
		List<String> log = getLog();
		Collection<Table> tables = BackupUtil.getTables();
		AbstractPersistence p = AbstractPersistence.get();
		String customerName = p.getUser().getCustomerName();
		
		String backupDir = String.format("%sbackup_%s%s%s%s",
											UtilImpl.CONTENT_DIRECTORY, 
											customerName, 
											File.separator, 
											ThreadSafeFactory.getDateFormat("yyyyMMddHHmmss").format(new java.util.Date()),
											File.separator);
		File directory = new File(backupDir);
		directory.mkdirs();
		String trace = "Backup to " + directory.getAbsolutePath();
		log.add(trace);
		UtilImpl.LOGGER.info(trace);

		BackupUtil.writeTables(tables, new File(backupDir, "tables.txt"));

		p.generateDDL(new File(backupDir, "drop.sql").getAbsolutePath(), 
						new File(backupDir, "create.sql").getAbsolutePath(),
						null);
		boolean problem = false; // indicates if the backup had a problem
		try {
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
										trace = "Backup " + table.name;
										log.add(trace);
										UtilImpl.LOGGER.info(trace);
										try (OutputStreamWriter out = new OutputStreamWriter(
												new FileOutputStream(backupDir + File.separator + table.name + ".csv"), UTF_8)) {
											try (CsvMapWriter writer = new CsvMapWriter(out, CsvPreference.STANDARD_PREFERENCE)) {
												Map<String, Object> values = new TreeMap<>();
												String[] headers = new String[table.fields.size()];
												headers = table.fields.keySet().toArray(headers);
				
												writer.writeHeader(headers);
				
												while (resultSet.next()) {
													if (isCancelled()) {
														return;
													}
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
														else if (AttributeType.content.equals(attributeType) ||
																	AttributeType.image.equals(attributeType)) {
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
																		problem = true;
																		problems.write(String.format("Table [%s] with [%s] = %s is missing content for attribute [%s] = %s",
																										table.name,
																										Bean.DOCUMENT_ID,
																										values.get(Bean.DOCUMENT_ID),
																										name,
																										stringValue));
																		// See if the content file exists 
																		final File contentDirectory = Paths.get(UtilImpl.CONTENT_DIRECTORY, AbstractContentManager.FILE_STORE_NAME).toFile();
																		final StringBuilder contentAbsolutePath = new StringBuilder(contentDirectory.getAbsolutePath()).append(File.separator);
																		AbstractContentManager.appendBalancedFolderPathFromContentId(stringValue, contentAbsolutePath, false);
																		final File contentFile = Paths.get(contentAbsolutePath.toString()).toFile();
																		if (contentFile.exists()) {
																			problems.write(" but the matching file was found for this missing content at ");
																			problems.write(contentFile.getAbsolutePath());
																		}
																		problems.newLine();
																	}
																	else {
																		StringBuilder contentPath = new StringBuilder(256);
																		contentPath.append(directory.getAbsolutePath()).append('/').append(AbstractContentManager.FILE_STORE_NAME).append('/');
																		ElasticContentManager.writeContentFiles(contentPath, content, content.getContentBytes());
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
								// log the offending SQL statement
								catch (SQLException e) {
									trace = "Failed SQL : " + sql.toString();
									problems.write(trace);
									problems.newLine();
									log.add(trace);
									Util.LOGGER.severe(trace);
									throw e;
								}
							}

							connection.commit();
						}
					}
					// log the exception in problems.txt on the way out
					catch (Exception e) {
						problems.write("A problem backing up was encountered : " + e.getLocalizedMessage());
						problems.newLine();
						throw e;
					}
				}
			}
		}
		catch (Exception e) {
			problem = true;
			trace = "A problem backing up was encountered : " + e.getLocalizedMessage();
			log.add(trace);
			Util.LOGGER.info(trace);
			throw e;
		}
		finally {
			if (directory.exists()) {
				trace = "Created backup folder " + directory.getAbsolutePath();
				log.add(trace);
				Util.LOGGER.info(trace);
				setPercentComplete(50);
				try {
					File zip = new File(directory.getParentFile(), 
											directory.getName() + (problem ? "_PROBLEMS.zip" : ".zip"));
					FileUtil.createZipArchive(directory, zip);
					trace = "Compressed backup to " + zip.getAbsolutePath();
					log.add(trace);
					Util.LOGGER.info(trace);
					backupZip = zip;
				}
				finally {
					FileUtil.delete(directory);
					trace = "Deleted backup folder " + directory.getAbsolutePath();
					log.add(trace);
					Util.LOGGER.info(trace);
					setPercentComplete(100);
					EXT.push(new PushMessage().user().growl(MessageSeverity.info, "Backup Completed" + (problem ? " with problems" : "")));
				}
			}
		}
	}
}
