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
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.app.admin.DataMaintenance;
import org.skyve.domain.app.admin.DataMaintenance.DataSensitivity;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.types.DateOnly;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.CancellableJob;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Binder;
import org.skyve.util.FileUtil;
import org.skyve.util.Mail;
import org.skyve.util.PushMessage;
import org.skyve.util.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.supercsv.io.CsvMapWriter;
import org.supercsv.prefs.CsvPreference;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
    
    private static final Logger SLOGGER = LoggerFactory.getLogger(BackupJob.class);

	private File backupZip;

	public File getBackupZip() {
		return backupZip;
	}

	@Override
	public void execute() throws Exception {
		Bean bean = getBean();
		List<String> log = getLog();
		Collection<Table> tables = BackupUtil.getTables();
		AbstractPersistence p = AbstractPersistence.get();
		Customer customer = p.getUser().getCustomer();
		String customerName = customer.getName();

		String backupDir = String.format("%sbackup_%s%s%s%s",
											Util.getBackupDirectory(),
											customerName,
											File.separator,
											CORE.getDateFormat("yyyyMMddHHmmss").format(new java.util.Date()),
											File.separator);
		File directory = new File(backupDir);
		directory.mkdirs();
		String trace = "Backup to " + directory.getAbsolutePath();
		String causation = null;
		log.add(trace);
		LOGGER.info(trace);
		
		// Are we including audits in this backup?
		boolean includeAuditLog = getIncludeAuditLog(bean);
		if (! includeAuditLog) {
			Module admin = customer.getModule(AppConstants.ADMIN_MODULE_NAME);
			Document audit = admin.getDocument(customer, AppConstants.AUDIT_DOCUMENT_NAME);
			Persistent persistent = audit.getPersistent();
			String auditPersistentIdentifier = (persistent == null) ? null : persistent.getPersistentIdentifier();
			tables.removeIf(t -> t.persistentIdentifier.equals(auditPersistentIdentifier));
		}
		
		// Are we including content in this backup?
		boolean includeContent = getIncludeContent(bean);
		
		// Determine level of redaction
		int sensitivityLevel = getSensitivityLevel(bean);
		
		BackupUtil.writeTables(tables, new File(backupDir, "tables.txt"));

		p.generateDDL(new File(backupDir, "drop.sql").getAbsolutePath(),
				new File(backupDir, "create.sql").getAbsolutePath(),
				null);
		boolean problem = false; // indicates if the backup had a problem
		try {
			try {
				try (FileWriter problemsTxt = new FileWriter(new File(backupDir, "problems.txt"))) {
					try (BufferedWriter problems = new BufferedWriter(problemsTxt)) {
						try (Connection connection = EXT.getDataStoreConnection()) {
							connection.setAutoCommit(false);
	
							try (ContentManager cm = EXT.newContentManager()) {
								for (Table table : tables) {
									StringBuilder sql = new StringBuilder(128);
									try (Statement statement = connection.createStatement()) {
										sql.append("select * from ").append(table.persistentIdentifier);
										BackupUtil.secureSQL(sql, table, customerName);
										statement.execute(sql.toString());
										try (ResultSet resultSet = statement.getResultSet()) {
											trace = "Backup " + table.agnosticIdentifier;
											log.add(trace);
											LOGGER.info(trace);
											try (OutputStreamWriter out = new OutputStreamWriter(
													new FileOutputStream(backupDir + File.separator + table.agnosticIdentifier + ".csv"), UTF_8)) {
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
															BackupField field = table.fields.get(name);
															AttributeType attributeType = field.getAttributeType();
															Sensitivity sensitivity = field.getSensitivity();
															boolean redact = (sensitivityLevel > 0) && (sensitivity.ordinal() >= sensitivityLevel);
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
																		throw new IllegalStateException(table.agnosticIdentifier + " is missing a " + Bean.DOCUMENT_ID + " value.");
																	}
																	// bizLock is mandatory
																	if (name.equalsIgnoreCase(PersistentBean.LOCK_NAME)) {
																		throw new IllegalStateException(table.agnosticIdentifier + " with " +
																											Bean.DOCUMENT_ID + " = " + values.get(Bean.DOCUMENT_ID) +
																											" is missing a " + PersistentBean.LOCK_NAME + " value.");
																	}
																	// bizKey is mandatory
																	if (name.equalsIgnoreCase(Bean.BIZ_KEY)) {
																		throw new IllegalStateException(table.agnosticIdentifier + " with " +
																											Bean.DOCUMENT_ID + " = " + values.get(Bean.DOCUMENT_ID) +
																											" is missing a " + Bean.BIZ_KEY + " value.");
																	}
																	// bizCustomer is mandatory
																	if (name.equalsIgnoreCase(Bean.CUSTOMER_NAME)) {
																		throw new IllegalStateException(table.agnosticIdentifier + " with " +
																											Bean.DOCUMENT_ID + " = " + values.get(Bean.DOCUMENT_ID) +
																											" is missing a " + Bean.CUSTOMER_NAME + " value.");
																	}
																	// bizUserId is mandatory
																	if (name.equalsIgnoreCase(Bean.USER_ID)) {
																		throw new IllegalStateException(table.agnosticIdentifier + " with " +
																											Bean.DOCUMENT_ID + " = " + values.get(Bean.DOCUMENT_ID) +
																											" is missing a " + Bean.USER_ID + " value.");
																	}
																}
																// Respect sensitivity
																if (redact) {
																	// Redact value
																	if (field instanceof BackupLengthField lengthField) {
																		value = BackupUtil.redactData(attributeType, value, lengthField.getMaxLength());
																	} else {
																		value = BackupUtil.redactData(attributeType, value);
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
																	// Respect sensitivity
																	if (redact) {
																		// Redact value
																		geometry = (Geometry) BackupUtil.redactData(attributeType, geometry);
																	}
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
																	// Respect sensitivity
																	if (redact) {
																		// Redact value
																		value = BackupUtil.redactData(attributeType, value);
																	}
																}
															}
															else if (AttributeType.date.equals(attributeType)) {
																Date date = resultSet.getDate(name, BackupUtil.GMT);
																if (resultSet.wasNull()) {
																	value = "";
																}
																else {
																	// Respect sensitivity
																	if (redact) {
																		// Redact value
																		date = (Date) BackupUtil.redactData(attributeType, date);
																	}
																	value = Long.valueOf(date.getTime());
																}
															}
															else if (AttributeType.time.equals(attributeType)) {
																Time time = resultSet.getTime(name, BackupUtil.GMT);
																if (resultSet.wasNull()) {
																	value = "";
																}
																else {
																	// Respect sensitivity
																	if (redact) {
																		// Redact value
																		time = (Time) BackupUtil.redactData(attributeType, time);
																	}
																	value = Long.valueOf(time.getTime());
																}
															}
															else if (AttributeType.dateTime.equals(attributeType) ||
																	AttributeType.timestamp.equals(attributeType)) {
																Timestamp timestamp = resultSet.getTimestamp(name, BackupUtil.GMT);
																if (resultSet.wasNull()) {
																	value = "";
																}
																else {
																	// Respect sensitivity
																	if (redact) {
																		// Redact value
																		timestamp = (Timestamp) BackupUtil.redactData(attributeType, timestamp);
																	}
																	value = Long.valueOf(timestamp.getTime());
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
																	// Respect sensitivity
																	if (redact) {
																		// Redact value
																		bigDecimal = (BigDecimal) BackupUtil.redactData(attributeType, bigDecimal);
																	}
																	value = bigDecimal;
																}
															}
															else if (AttributeType.integer.equals(attributeType)) {
																int intValue = resultSet.getInt(name);
																if (resultSet.wasNull()) {
																	value = "";
																}
																else {
																	value = Integer.valueOf(intValue);
																	// Respect sensitivity
																	if (redact) {
																		// Redact value
																		value = BackupUtil.redactData(attributeType, value);
																	}
																}
																// bizVersion is mandatory
																if ("".equals(value) &&
																		name.equalsIgnoreCase(PersistentBean.VERSION_NAME)) {
																	throw new IllegalStateException(table.agnosticIdentifier + " with " +
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
																	value = Long.valueOf(longValue);
																	// Respect sensitivity
																	if (redact) {
																		// Redact value
																		value = BackupUtil.redactData(attributeType, value);
																	}
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
																	// Redacting or excluding content will include content IDs but no content.
																	// This allows required content and workflow around content presence to continue to work.
																	// The restore options allow for clearing content IDs on restore if required.
																	if (includeContent && (! redact)) {
																		AttachmentContent content = null;
																		try {
																			content = cm.getAttachment(stringValue);
																			if (content == null) {
																				problem = true;
																				problems.write(String.format("Table [%s] with [%s] = %s is missing content for attribute [%s] = %s",
																						table.agnosticIdentifier,
																						Bean.DOCUMENT_ID,
																						values.get(Bean.DOCUMENT_ID),
																						name,
																						stringValue));
																				// See if the content file exists
																				final File contentDirectory = Paths.get(UtilImpl.CONTENT_DIRECTORY, ContentManager.FILE_STORE_NAME).toFile();
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
																				contentPath.append(directory.getAbsolutePath()).append('/').append(ContentManager.FILE_STORE_NAME).append('/');
																				AbstractContentManager.writeContentFiles(contentPath, content, content.getContentBytes());
																			}
																		}
																		catch (Throwable t) {
																			if (t instanceof FileNotFoundException) {
																				problems.write(String.format("Table [%s] with [%s] = %s is missing a file in the content store for attribute [%s] = %s",
																						table.agnosticIdentifier,
																						Bean.DOCUMENT_ID,
																						values.get(Bean.DOCUMENT_ID),
																						name,
																						stringValue));
																				problems.newLine();
																			}
																			else {
																				throw t;
																			}
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
										LOGGER.error(trace);
										throw e;
									}
								}
	
								connection.commit();
							}
						}
						// log the exception in problems.txt on the way out
						catch (Throwable t) {
							problems.write("A problem backing up was encountered : " + t.getLocalizedMessage());
							problems.newLine();
							throw t;
						}
					}
				}
			}
			catch (Throwable t) {
				problem = true;
				trace = "A problem backing up " + UtilImpl.ARCHIVE_NAME + " was encountered : " + t.getLocalizedMessage();
				causation = trace;
				log.add(trace);
				LOGGER.info(trace);
				throw t;
			}
			finally {
				if (directory.exists()) {
					trace = "Created backup folder " + directory.getAbsolutePath();
					log.add(trace);
					LOGGER.info(trace);
					setPercentComplete(50);
					try {
						File zip = new File(directory.getParentFile(),
								directory.getName() + (problem ? "_PROBLEMS.zip" : ".zip"));
						FileUtil.createZipArchive(directory, zip);
						trace = "Compressed backup to " + zip.getAbsolutePath();
						log.add(trace);
						LOGGER.info(trace);
						backupZip = zip;
	
						if (ExternalBackup.areExternalBackupsEnabled()) {
							ExternalBackup.getInstance().uploadBackup(zip.getAbsolutePath());
							final String uploadLogMessage = "Uploaded compressed backup";
							log.add(uploadLogMessage);
							LOGGER.info(uploadLogMessage);
	
							FileUtil.delete(zip);
							final String deleteLogMessage = "Deleted local backup";
							log.add(deleteLogMessage);
							LOGGER.info(deleteLogMessage);
						}
					}
					catch (Throwable t) {
						problem = true;
						trace = "A problem backing up " + UtilImpl.ARCHIVE_NAME + " was encountered : " + t.getLocalizedMessage();
						if (causation == null) {
							causation = trace;
						}
						log.add(trace);
						LOGGER.info(trace);
						throw t;
					}
					finally {
						FileUtil.delete(directory);
						trace = "Deleted backup folder " + directory.getAbsolutePath();
						log.add(trace);
						LOGGER.info(trace);
						setPercentComplete(100);
						trace = "Backup Completed" + (problem ? " with problems" : "");
						log.add(trace);
						LOGGER.info(trace);
						EXT.push(new PushMessage().user().growl(MessageSeverity.info, trace));
					}
				}
			}
		}
		finally {
			if (problem) {
				emailProblem(log, causation);
			}
		}
	}
	
	public static void emailProblem(@Nonnull List<String> jobLog, @Nullable String problem) throws Exception {
		String body = Binder.formatMessage("The " + UtilImpl.ARCHIVE_NAME + " backup taken at " + new DateOnly() + " has ");
		if (problem == null) {
			body += "problems.";
		}
		else {
			body += "a problem:- " + problem;
		}

		if (UtilImpl.SUPPORT_EMAIL_ADDRESS != null) {
			EXT.sendMail(new Mail().from(UtilImpl.SMTP_SENDER)
									.addTo(UtilImpl.SUPPORT_EMAIL_ADDRESS)
									.subject("Problems with recent backup.")
									.body(body));
		}
		else {
			String trace = "Could not send a backup problem email as there is not a support email address defined - " + body;
			jobLog.add(trace);
			SLOGGER.info(trace);
		}
	}

	/**
	 * Fetch sensitivity level, calculated from ordinal value of {@link SensitivityType} selected in UI.
	 * 
	 * Returns 0 if no sensitivity level is selected.
	 * 
	 * @param bean DataMaintenance bean
	 */
	private static int getSensitivityLevel(Bean bean) {
		if (bean instanceof DataMaintenance dataMaintenance) {
			DataSensitivity sensitivityInput = dataMaintenance.getDataSensitivity();
			if (sensitivityInput != null) {
				return Sensitivity.valueOf(sensitivityInput.toString()).ordinal();
			}
		}
		
		return 0;
	}
	
	/**
	 * Fetch 'include content' value selected in UI.
	 * 
	 * @param bean DataMaintenance bean
	 */
	private static boolean getIncludeContent(Bean bean) {
		if (bean instanceof DataMaintenance dataMaintenance) {
			Boolean includeContent = dataMaintenance.getIncludeContent();
			return Boolean.TRUE.equals(includeContent);
		}
		
		return true; // content included by default
	}
	
	/**
	 * Fetch 'include audits' value selected in UI.
	 * 
	 * @param bean DataMaintenance bean
	 */
	private static boolean getIncludeAuditLog(Bean bean) {
		if (bean instanceof DataMaintenance dataMaintenance) {
			Boolean includeAudits = dataMaintenance.getIncludeAuditLog();
			return Boolean.TRUE.equals(includeAudits);
		}
		
		return true; // audits included by default
	}
}
