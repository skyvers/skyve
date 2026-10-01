package org.skyve.impl.backup;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
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
import java.util.Calendar;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.TreeMap;

import org.apache.commons.io.FileUtils;
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
import org.skyve.impl.metadata.customer.CustomerImpl;
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
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;
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
	private static final Logger SLOGGER = SkyveLoggerFactory.getLogger(BackupJob.class);

	private static final String CREATE_SQL = "create.sql";
	private static final String PROBLEMS_TXT = "problems.txt";
	private static final int MAX_LOGGED_PROBLEMS = 100;
	private static final String FIELD_VALUE_SUFFIX = " value.";
	private static final String MISSING_FIELD_PREFIX = " is missing a ";
	private static final String WITH_DOCUMENT_ID = " with " + Bean.DOCUMENT_ID + " = ";

	private Calendar gmt = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

	private File backupZip;

	/**
	 * Return the generated backup zip for this job, if available.
	 *
	 * @return the backup zip file or null if not generated yet
	 */
	public File getBackupZip() {
		return backupZip;
	}

	/**
	 * Run the backup job.
	 *
	 * @throws Exception if the backup fails
	 */
	@Override
	public void execute() throws Exception {
		CustomerImpl customer = (CustomerImpl) CORE.getCustomer();
		try {
			// Notify observers that we are starting a backup for this customer
			customer.notifyBeforeBackup();

			backup();
		}
		finally {
			// Notify observers that we are finished a backup for this customer
			customer.notifyAfterBackup();
		}
	}
	
	/**
	 * Perform the backup workflow and write the backup archive.
	 *
	 * @throws Exception if the backup fails
	 */
	@SuppressWarnings({"java:S1143", "java:S3776", "java:S6541"}) // Allow nested try blocks for clarity in resource management and error handling; complexity OK
	private void backup() throws Exception {
		long start = System.currentTimeMillis();
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
		trace = "Usable space on backup volume " + formatSize(directory.getUsableSpace());
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
		trace = String.format("Backup options: include audit log = %s, include content = %s, redaction = %s",
								Boolean.valueOf(includeAuditLog),
								Boolean.valueOf(includeContent),
								Sensitivity.values()[sensitivityLevel]);
		log.add(trace);
		LOGGER.info(trace);
		
		BackupUtil.writeTables(tables, new File(backupDir, "tables.txt"));

		List<String> dropDDL = new java.util.ArrayList<>();
		List<String> createDDL = new java.util.ArrayList<>();
		p.generateDDL(dropDDL, createDDL, null);
		BackupUtil.writeScript(dropDDL, new File(backupDir, "drop.sql"));
		BackupUtil.writeScript(createDDL, new File(backupDir, CREATE_SQL));
		boolean problem = false; // indicates if the backup had a problem
		int problemCount = 0;
		try {
			try {
				try (FileWriter problemsTxt = new FileWriter(new File(backupDir, PROBLEMS_TXT))) {
					try (BufferedWriter problems = new BufferedWriter(problemsTxt)) {
						try (Connection connection = EXT.getDataStoreConnection()) {
							connection.setAutoCommit(false);
	
							try (ContentManager cm = EXT.newContentManager()) {
								long exportStart = System.currentTimeMillis();
								int contentFiles = 0;
								long contentBytes = 0;
								for (Table table : tables) {
									long tableStart = System.currentTimeMillis();
									int rows = 0;
									StringBuilder sql = new StringBuilder(128);
									try (Statement statement = connection.createStatement()) {
										sql.append("select * from ").append(table.persistentIdentifier);
										BackupUtil.secureSQL(sql, table, customerName);
										statement.execute(sql.toString());
										try (ResultSet resultSet = statement.getResultSet()) {
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
														rows++;
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
																		throw new IllegalStateException(table.agnosticIdentifier + MISSING_FIELD_PREFIX + Bean.DOCUMENT_ID + FIELD_VALUE_SUFFIX);
																	}
																	// bizLock is mandatory
																	if (name.equalsIgnoreCase(PersistentBean.LOCK_NAME)) {
																		throw new IllegalStateException(table.agnosticIdentifier + WITH_DOCUMENT_ID + values.get(Bean.DOCUMENT_ID) +
																											MISSING_FIELD_PREFIX + PersistentBean.LOCK_NAME + FIELD_VALUE_SUFFIX);
																	}
																	// bizKey is mandatory
																	if (name.equalsIgnoreCase(Bean.BIZ_KEY)) {
																		throw new IllegalStateException(table.agnosticIdentifier + WITH_DOCUMENT_ID + values.get(Bean.DOCUMENT_ID) +
																											MISSING_FIELD_PREFIX + Bean.BIZ_KEY + FIELD_VALUE_SUFFIX);
																	}
																	// bizCustomer is mandatory
																	if (name.equalsIgnoreCase(Bean.CUSTOMER_NAME)) {
																		throw new IllegalStateException(table.agnosticIdentifier + WITH_DOCUMENT_ID + values.get(Bean.DOCUMENT_ID) +
																											MISSING_FIELD_PREFIX + Bean.CUSTOMER_NAME + FIELD_VALUE_SUFFIX);
																	}
																	// bizUserId is mandatory
																	if (name.equalsIgnoreCase(Bean.USER_ID)) {
																		throw new IllegalStateException(table.agnosticIdentifier + WITH_DOCUMENT_ID + values.get(Bean.DOCUMENT_ID) +
																											MISSING_FIELD_PREFIX + Bean.USER_ID + FIELD_VALUE_SUFFIX);
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
																Date date = resultSet.getDate(name, gmt);
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
																Time time = resultSet.getTime(name, gmt);
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
																Timestamp timestamp = resultSet.getTimestamp(name, gmt);
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
																	throw new IllegalStateException(table.agnosticIdentifier + WITH_DOCUMENT_ID + values.get(Bean.DOCUMENT_ID) +
																			MISSING_FIELD_PREFIX + PersistentBean.VERSION_NAME + FIELD_VALUE_SUFFIX);
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
																				AbstractContentManager.appendBalancedFolderPathFromContentId(stringValue, contentAbsolutePath);
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
																				try (InputStream cs = content.getContentStream()) {
																					AbstractContentManager.writeContentFiles(contentPath, content, cs, true);
																				}
																				contentFiles++;
																				contentBytes += content.getContentLength();
																			}
																		}
																		catch (Throwable t) {
																			if (t instanceof FileNotFoundException) {
																				problem = true;
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
										trace = String.format("Backup %s - %,d rows in %s",
																table.agnosticIdentifier, Integer.valueOf(rows), elapsed(tableStart));
										log.add(trace);
										LOGGER.info(trace);
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
								trace = String.format("Exported %,d tables and %,d content files (%s) in %s",
														Integer.valueOf(tables.size()),
														Integer.valueOf(contentFiles),
														formatSize(contentBytes),
														elapsed(exportStart));
								log.add(trace);
								LOGGER.info(trace);
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
				LOGGER.error(trace);
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
						long zipStart = System.currentTimeMillis();
						FileUtil.createZipArchive(directory, zip);
						trace = "Compressed backup to " + zip.getAbsolutePath() + " in " + elapsed(zipStart);
						log.add(trace);
						LOGGER.info(trace);
						backupZip = zip;
	
						// Peak disk usage - the backup folder and the zip both exist
						long unzippedSize = FileUtils.sizeOfDirectory(directory);
						long zippedSize = zip.length();
						trace = String.format("Backup size %s unzipped, %s zipped (%.1f%% of unzipped)",
												formatSize(unzippedSize),
												formatSize(zippedSize),
												Double.valueOf((unzippedSize == 0) ? 0 : (100.0 * zippedSize / unzippedSize)));
						log.add(trace);
						LOGGER.info(trace);
						long usableSpace = directory.getUsableSpace();
						trace = "Usable space on backup volume after compression " + formatSize(usableSpace);
						log.add(trace);
						LOGGER.info(trace);
						if (usableSpace < unzippedSize + zippedSize) {
							trace = String.format("Usable space on backup volume %s is less than the %s the next backup needs (unzipped + zipped)",
													formatSize(usableSpace),
													formatSize(unzippedSize + zippedSize));
							log.add(trace);
							LOGGER.warn(trace);
						}
	
						if (ExternalBackup.areExternalBackupsEnabled()) {
							long uploadStart = System.currentTimeMillis();
							ExternalBackup.getInstance().uploadBackup(zip.getAbsolutePath());
							trace = String.format("Uploaded compressed backup %s (%s) in %s",
													zip.getName(), formatSize(zippedSize), elapsed(uploadStart));
							log.add(trace);
							LOGGER.info(trace);
	
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
						LOGGER.error(trace);
						throw t;
					}
					finally {
						problemCount = logProblems(new File(directory, PROBLEMS_TXT), log);
						FileUtil.delete(directory);
						trace = "Deleted backup folder " + directory.getAbsolutePath();
						log.add(trace);
						LOGGER.info(trace);
						setPercentComplete(100);
						trace = String.format("Backup Completed%s - %s in %s",
												problem ? String.format(" with %,d problems", Integer.valueOf(problemCount)) : "",
												(backupZip == null) ? "no backup file" : backupZip.getName(),
												elapsed(start));
						log.add(trace);
						if (problem) {
							LOGGER.warn(trace);
						}
						else {
							LOGGER.info(trace);
						}
						EXT.push(new PushMessage().user().growl(problem ? MessageSeverity.warn : MessageSeverity.info, trace));
					}
				}
			}
		}
		finally {
			if (problem) {
				String details = String.format("%,d problems recorded%s.",
												Integer.valueOf(problemCount),
												(backupZip == null) ? "" : " in backup " + backupZip.getName());
				emailProblem(log, (causation == null) ? details : causation + ". " + details);
			}
		}
	}
	
	/**
	 * Email a backup problem report to support.
	 *
	 * @param jobLog the job log to append messages to
	 * @param problem the problem description, or null for a generic message
	 * @throws Exception if sending the email fails
	 */
	public static void emailProblem(@Nonnull List<String> jobLog, @Nullable String problem) {
		// nameEnv is the application name and environment identifier.
		StringBuilder nameEnv = new StringBuilder();
		nameEnv.append("[").append(UtilImpl.ARCHIVE_NAME);
		if (UtilImpl.ENVIRONMENT_IDENTIFIER != null) {
			nameEnv.append(" - ").append(UtilImpl.ENVIRONMENT_IDENTIFIER);
		}
		nameEnv.append("]");
		String body = Binder.formatMessage("The " + nameEnv + " backup taken at " + new DateOnly() + " has ");
		if (problem == null) {
			body += "problems.";
		}
		else {
			body += "a problem:- " + problem;
		}
		body += " See the backup job log (admin -> Jobs) for details.";

		StringBuilder subjectBuilder = new StringBuilder();
		subjectBuilder.append(nameEnv).append(" Backup Problem");

		if (UtilImpl.SUPPORT_EMAIL_ADDRESS != null) {
			EXT.getMailService()
					.sendMail(new Mail().from(UtilImpl.SMTP_SENDER)
									.addTo(UtilImpl.SUPPORT_EMAIL_ADDRESS)
									.subject(subjectBuilder.toString())
									.body(body));
		}
		else {
			String trace = "Could not send a backup problem email as there is not a support email address defined - " + body;
			jobLog.add(trace);
			SLOGGER.info(trace);
		}
	}

	/**
	 * Logs a summary of the problems recorded in problems.txt followed by the problems themselves,
	 * capped at {@link #MAX_LOGGED_PROBLEMS} lines, so a backup can be triaged from the job log.
	 * Nothing is logged when there are no problems.
	 *
	 * @param problemsTxt the problems.txt file written by the backup
	 * @param jobLog the job log to append messages to
	 * @return the number of problems recorded
	 * @throws IOException if problems.txt cannot be read
	 */
	static int logProblems(@Nonnull File problemsTxt, @Nonnull List<String> jobLog) throws IOException {
		List<String> logged = new java.util.ArrayList<>();
		int count = 0;
		try (BufferedReader reader = new BufferedReader(new FileReader(problemsTxt))) {
			String line;
			while ((line = reader.readLine()) != null) {
				if (count++ < MAX_LOGGED_PROBLEMS) {
					logged.add(line);
				}
			}
		}
		if (count > MAX_LOGGED_PROBLEMS) {
			logged.add(String.format("... and %,d more", Integer.valueOf(count - MAX_LOGGED_PROBLEMS)));
		}
		if (count > 0) {
			logged.add(0, String.format("%,d problems recorded in %s", Integer.valueOf(count), PROBLEMS_TXT));
			for (String trace : logged) {
				jobLog.add(trace);
				SLOGGER.warn(trace);
			}
		}
		return count;
	}

	/**
	 * Formats a number of bytes in megabytes for the job log.
	 *
	 * @param bytes the number of bytes
	 * @return the size in MB to 1 decimal place
	 */
	private static String formatSize(long bytes) {
		return String.format("%,.1f MB", Double.valueOf((double) bytes / Util.MEGABYTE));
	}

	/**
	 * Formats the time elapsed since a start time for the job log.
	 *
	 * @param startMillis the start time in epoch milliseconds
	 * @return the elapsed time in seconds to 1 decimal place
	 */
	private static String elapsed(long startMillis) {
		return String.format("%,.1f s", Double.valueOf((System.currentTimeMillis() - startMillis) / 1000.0));
	}

	/**
	 * Fetch sensitivity level, calculated from ordinal value of {@link SensitivityType} selected in UI.
	 *
	 * Returns 0 if no sensitivity level is selected.
	 *
	 * @param bean DataMaintenance bean
	 * @return the sensitivity level ordinal
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
	 * @return true if content should be included
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
	 * @return true if audit log should be included
	 */
	private static boolean getIncludeAuditLog(Bean bean) {
		if (bean instanceof DataMaintenance dataMaintenance) {
			Boolean includeAudits = dataMaintenance.getIncludeAuditLog();
			return Boolean.TRUE.equals(includeAudits);
		}
		
		return true; // audits included by default
	}
}
