package org.skyve.impl.backup;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.Time;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.Collection;
import java.util.Map;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.WKTReader;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.backup.RestoreOptions.ContentOption;
import org.skyve.impl.backup.RestoreOptions.IndexingOption;
import org.skyve.impl.backup.RestoreOptions.PreProcess;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.CancellableJob;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.FileUtil;
import org.skyve.util.PushMessage;
import org.skyve.util.Util;
import org.supercsv.io.CsvMapReader;
import org.supercsv.prefs.CsvPreference;

public class RestoreJob extends CancellableJob {
	/**
	 * Executes a restore job when invoked with {@link RestoreOptions}.
	 * Logs a user-facing message if the job is triggered with the wrong bean type.
	 */
	@Override
	public void execute() throws Exception {
		Bean bean = getBean();
		if (! (bean instanceof RestoreOptions)) {
			getLog().add("Kick off the job with the appropriate options from the Data Maintenance page.");
			return;
		}
		restore((RestoreOptions) bean);
	}

	/**
	 * Orchestrates a full restore from the selected backup, including optional DDL work,
	 * data restore, indexing, and cleanup.
	 */
	private void restore(RestoreOptions options) throws Exception {
		CustomerImpl customer = (CustomerImpl) CORE.getCustomer();
		String customerName = customer.getName();

		// Notify observers that we are starting a restore for this customer
		customer.notifyBeforeRestore();

		Collection<String> log = getLog();
		String trace;

		String selectedBackupName = options.getSelectedBackupName();
		Path backupDir = Paths.get(Util.getBackupDirectory(), "backup_" + customerName);
		File backup = backupDir.resolve(selectedBackupName).toFile();
		boolean deleteLocalBackup = false;
		boolean restoreSuccessful = true;
		
		try {
			if (ExternalBackup.areExternalBackupsEnabled()) {
				deleteLocalBackup = true;
				backupDir.toFile().mkdirs();
				try (final FileOutputStream backupOutputStream = new FileOutputStream(backup)) {
					LOGGER.info("Downloading external backup {}", backup.getName());
					ExternalBackup.getInstance().downloadBackup(selectedBackupName, backupOutputStream);
				}
			}
			if (! backup.exists()) {
				trace = "Backup " + backup.getAbsolutePath() + " does not exist.";
				log.add(trace);
				LOGGER.warn(trace);
				return;
			}

			EXT.push(new PushMessage().growl(MessageSeverity.info,
												"System Restore in progress - system unavailable until restore is complete."));

			String extractDirName = selectedBackupName.substring(0, selectedBackupName.length() - 4);
			File extractDir = new File(backup.getParentFile(), extractDirName);
			trace = String.format("Extract %s to %s", backup.getAbsolutePath(), extractDir.getAbsolutePath());
			log.add(trace);
			LOGGER.info(trace);
			if (extractDir.exists()) {
				trace = String.format("    %s already exists - delete it.", extractDir.getAbsolutePath());
				log.add(trace);
				LOGGER.info(trace);
				FileUtil.delete(extractDir);
				trace = String.format("    %s deleted.", extractDir.getAbsolutePath());
				log.add(trace);
				LOGGER.info(trace);
			}
			FileUtil.extractZipArchive(backup, extractDir);
			trace = String.format("Extracted %s to %s", backup.getAbsolutePath(), extractDir.getAbsolutePath());
			log.add(trace);
			LOGGER.info(trace);
			setPercentComplete(50);

			File validatedBackup = BackupUtil.validateSkyveBackup(extractDirName);

			PreProcess restorePreProcess = options.getPreProcess();
			ContentOption contentRestoreOption = options.getContentOption();

			boolean truncateDatabase = PreProcess.deleteData.equals(restorePreProcess);
			if (truncateDatabase) {
				trace = "Truncate " + ((UtilImpl.SCHEMA == null) ? "default" : UtilImpl.SCHEMA) + " schema";
				log.add(trace);
				LOGGER.info(trace);
			}
			Truncate.truncate(UtilImpl.SCHEMA, truncateDatabase, true);

			boolean createUsingBackup = false;
			boolean ddlSync = false;
			if (PreProcess.createUsingBackup.equals(restorePreProcess)) {
				createUsingBackup = true;
				DDL.create(new File(extractDir, "create.sql"), true);
				ddlSync = true;
			}
			else if (PreProcess.createUsingMetadata.equals(restorePreProcess)) {
				DDL.create(null, true);
				ddlSync = true;
			}
			else if (PreProcess.dropUsingBackupAndCreateUsingBackup.equals(restorePreProcess)) {
				createUsingBackup = true;
				DDL.drop(new File(extractDir, "drop.sql"), true);
				DDL.create(new File(extractDir, "create.sql"), true);
				ddlSync = true;
			}
			else if (PreProcess.dropUsingBackupAndCreateUsingMetadata.equals(restorePreProcess)) {
				DDL.drop(new File(extractDir, "drop.sql"), true);
				DDL.create(null, true);
				ddlSync = true;
			}
			else if (PreProcess.dropUsingMetadataAndCreateUsingBackup.equals(restorePreProcess)) {
				createUsingBackup = true;
				DDL.drop(null, true);
				DDL.create(new File(extractDir, "create.sql"), true);
				ddlSync = true;
			}
			else if (PreProcess.dropUsingMetadataAndCreateUsingMetadata.equals(restorePreProcess)) {
				DDL.drop(null, true);
				DDL.create(null, true);
				ddlSync = true;
			}

			trace = "Restore " + extractDirName;
			log.add(trace);
			LOGGER.info(trace);
			IndexingOption indexingOption = options.getIndexingOption();
			restore(validatedBackup, createUsingBackup, contentRestoreOption, indexingOption);
			if (ddlSync) {
				trace = "DDL Sync";
				log.add(trace);
				LOGGER.info(trace);
				DDL.sync(true);
			}
			if (IndexingOption.both.equals(indexingOption) || IndexingOption.data.equals(indexingOption)) {
				trace = "Reindex textual indexes.";
				log.add(trace);
				LOGGER.info(trace);
				execute(new ReindexBeansJob());
			}

			trace = "Delete extracted folder " + extractDir.getAbsolutePath();
			log.add(trace);
			LOGGER.info(trace);
			FileUtil.delete(extractDir);
			trace = "DONE";
			log.add(trace);
			LOGGER.info(trace);
			setPercentComplete(100);

			EXT.push(new PushMessage().growl(MessageSeverity.info, "System Restore complete."));
		}
		catch (Throwable t) {
			restoreSuccessful = false;
			throw t;
		}
		finally {
			try {
				if (deleteLocalBackup) {
					if (! backup.delete()) {
						LOGGER.warn("Failed to delete local backup {}", backup.getAbsolutePath());
					}
				}
			}
			finally {
				try {
					EXT.getJobScheduler().postRestore(restoreSuccessful);
				}
				finally {
					// Notify observers that we are finished a restore for this customer
					customer.notifyAfterRestore();
				}
			}
		}
	}

	/**
	 * Restores data and foreign keys from a validated backup directory.
	 *
	 * @param backupDirectory The extracted backup folder containing CSV data.
	 * @param createUsingBackup Whether table metadata should be read from the backup.
	 * @param contentRestoreOption How to handle missing content on restore.
	 * @param indexingOption Which content/text indexes to rebuild.
	 */
	private void restore(File backupDirectory,
							boolean createUsingBackup,
							ContentOption contentRestoreOption,
							IndexingOption indexingOption)
	throws Exception {
		Collection<Table> tables = createUsingBackup ?
									BackupUtil.readTables(new File(backupDirectory, "tables.txt")) :
									BackupUtil.getTables();

		try (Connection connection = EXT.getDataStoreConnection()) {
			connection.setAutoCommit(false);

			// restore normal tables
			restoreData(backupDirectory, tables, connection, false, false, contentRestoreOption, indexingOption);
			setPercentComplete(25);
			// restore extension join tables
			restoreData(backupDirectory, tables, connection, false, true, contentRestoreOption, indexingOption);
			setPercentComplete(50);
			// link foreign keys
			restoreForeignKeys(backupDirectory, tables, connection);
			setPercentComplete(75);
			// restore collection join tables
			restoreData(backupDirectory, tables, connection, true, false, contentRestoreOption, indexingOption);
			setPercentComplete(100);
		}
	}

//	update bizKeys
//	check and remove content not present
//	validate by updating bizLock and rolling back
//	check commit points

	/**
	 * Restores table data from CSV files, handling join tables, extension tables,
	 * and content attachments according to restore options.
	 */
	private void restoreData(File backupDirectory,
								Collection<Table> tables,
								Connection connection,
								boolean joinTables,
								boolean extensionTables,
								ContentOption contentRestoreOption,
								IndexingOption indexingOption)
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
				Collection<String> log = getLog();
				String trace = "    restore table " + table.agnosticIdentifier;
				log.add(trace);
				LOGGER.info(trace);
				File backupFile = new File(backupDirectory.getAbsolutePath() + File.separator + table.agnosticIdentifier + ".csv");
				if (! backupFile.exists()) {
					trace = "        ***** File " + backupFile.getAbsolutePath() + " does not exist";
					log.add(trace);
					System.err.println(trace);
					continue;
				}

				long rowCount = 0;

				try (InputStreamReader in = new InputStreamReader(new FileInputStream(backupFile), StandardCharsets.UTF_8)) {
					try (CsvMapReader reader = new CsvMapReader(in, CsvPreference.STANDARD_PREFERENCE)) {
						String[] headers = reader.getHeader(true);

						StringBuilder sql = new StringBuilder(128);
						sql.append("insert into ").append(table.persistentIdentifier).append(" (");
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
								if (isCancelled()) {
									return;
								}

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

									BackupField field = table.fields.get(header);
									AttributeType attributeType = (field == null) ? null : field.getAttributeType();

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
									else if (AttributeType.content.equals(attributeType) ||
												AttributeType.image.equals(attributeType)) {
										StringBuilder contentPath = new StringBuilder(128);
										contentPath.append(backupDirectory.getAbsolutePath()).append('/');
										contentPath.append(ContentManager.FILE_STORE_NAME).append('/');

										AttachmentContent content = AbstractContentManager.getFromFileSystem(contentPath, stringValue, true);
										if (content == null) {
											trace = "        Could not find file associated with " + stringValue;
											if (ContentOption.error.equals(contentRestoreOption)) {
												log.add(trace);
												LOGGER.error(trace);
												throw new DomainException(trace);
											}
											else if (ContentOption.clearOrphanedContentIds.equals(contentRestoreOption)) {
												trace += " : Setting content to null";
											 log.add(trace);
												LOGGER.info(trace);
												statement.setString(index++, null);
											}
											else {
												trace += " : Setting content ID regardless";
											 log.add(trace);
												LOGGER.info(trace);
												statement.setString(index++, stringValue);
											}
										}
										else {
											IndexType indexType = table.indexes.get(header);
											boolean textIndex = (indexType == null) ||
																	IndexType.textual.equals(indexType) ||
																	IndexType.both.equals(indexType);
											if (textIndex) {
												textIndex = IndexingOption.both.equals(indexingOption) ||
																IndexingOption.content.equals(indexingOption);
											}
											cm.put(content, textIndex);
											statement.setString(index++, content.getContentId());
										}
									}
									else {
										trace = "RestoreJob unknown attribute type " + attributeType + " for column " + header;
										LOGGER.error(trace);
										// dump the field map for this table
										table.fields.entrySet()
												.stream()
												.forEach(e -> LOGGER.warn("    Table {}.{} -> {}", table.agnosticIdentifier, e.getKey(), e.getValue()));
										throw new IllegalStateException(trace);
									}
								} // for (each header)

								statement.executeUpdate();
								rowCount++;

								if ((rowCount % 1000L) == 0L) {
									connection.commit();
									if ((rowCount % 10000L) == 0L) {
										LOGGER.info("      processed {} rows", Long.valueOf(rowCount));
									}
								}
							} // while (each CSV line)

							connection.commit();
						}
						catch (Throwable t) {
							trace = t.getLocalizedMessage();
							log.add(trace);
							LOGGER.error(trace);
							trace = "AT LINE " + rowCount + " OF " + backupFile.getAbsolutePath();
							log.add(trace);
							LOGGER.error(trace);
							trace = "CAUSED BY:- " + sql.toString();
							log.add(trace);
							LOGGER.error(trace);

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
							trace = sb.toString();
							log.add(trace);
							LOGGER.error(trace);

							throw t;
						}
					}
				}
				trace = "    restored table " + table.agnosticIdentifier + " with " + rowCount + " rows.";
				log.add(trace);
				LOGGER.info(trace);
			} // for (each table)
		}
	}

	/**
	 * Restores foreign keys for non-join tables after base data is loaded.
	 */
	private void restoreForeignKeys(File backupDirectory,
										Collection<Table> tables,
										Connection connection)
			throws Exception {
		Collection<String> log = getLog();
		String trace;

		for (Table table : tables) {
			if (table instanceof JoinTable) {
				continue;
			}
			trace = "    restore foreign keys for table " + table.agnosticIdentifier;
			log.add(trace);
			LOGGER.info(trace);
			File backupFile = new File(backupDirectory.getAbsolutePath() + File.separator + table.agnosticIdentifier + ".csv");
			if (! backupFile.exists()) {
				trace = "        ***** File " + backupFile.getAbsolutePath() + File.separator + " does not exist";
				log.add(trace);
				System.err.println(trace);
				continue;
			}

			long rowCount = 0;

			try (FileReader fr = new FileReader(backupFile)) {
				try (CsvMapReader reader = new CsvMapReader(fr, CsvPreference.STANDARD_PREFERENCE)) {
					String[] headers = reader.getHeader(true);

					StringBuilder sql = new StringBuilder(128);
					sql.append("update ").append(table.persistentIdentifier);
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
								if (isCancelled()) {
									return;
								}

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
								rowCount++;

								if ((rowCount % 1000L) == 0L) {
									connection.commit();
									if ((rowCount % 10000L) == 0L) {
										LOGGER.info("      processed {} rows", Long.valueOf(rowCount));
									}
								}
							} // while (each CSV line)

							connection.commit();
						}
					}
				}
			}
			trace = "    restored foreign keys for table " + table.agnosticIdentifier + " with " + rowCount + " rows.";
			log.add(trace);
			LOGGER.info(trace);
		} // for (each table)
	}
}
