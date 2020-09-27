package org.skyve.impl.backup;

import java.io.*;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
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
import org.skyve.impl.content.elastic.ElasticContentManager;
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
	private static final long serialVersionUID = -4076693395300706664L;

	@Override
	public void execute() throws Exception {
		Bean bean = getBean();
		if (! (bean instanceof RestoreOptions)) {
			getLog().add("Kick off the job with the appropriate options from the Data Maintenance page.");
			return;
		}
		restore((RestoreOptions) bean);
	}

	private void restore(RestoreOptions options)
	throws Exception {
		String customerName = CORE.getUser().getCustomerName();
		Collection<String> log = getLog();
		String trace;

		String selectedBackupName = options.getSelectedBackupName();
		File backup = new File(String.format("%sbackup_%s%s%s",
												Util.getContentDirectory(),
												customerName,
												File.separator,
												selectedBackupName));
		if (ExternalBackup.areExternalBackupsEnabled()) {
			ExternalBackup.getInstance().downloadBackup(selectedBackupName, new FileOutputStream(backup));
			backup.deleteOnExit();
		}
		if (! backup.exists()) {
			trace = "Backup " + backup.getAbsolutePath() + " does not exist.";
			log.add(trace);
			Util.LOGGER.warning(trace);
			return;
		}

		EXT.push(new PushMessage().growl(MessageSeverity.info, "System Restore in progress - system unavailable until restore is complete."));

		String extractDirName = selectedBackupName.substring(0, selectedBackupName.length() - 4);
		File extractDir = new File(backup.getParentFile(), extractDirName);
		trace = String.format("Extract %s to %s", backup.getAbsolutePath(), extractDir.getAbsolutePath());
		log.add(trace);
		Util.LOGGER.info(trace);
		if (extractDir.exists()) {
			trace = String.format("    %s already exists - delete it.", extractDir.getAbsolutePath());
			log.add(trace);
			Util.LOGGER.info(trace);
			FileUtil.delete(extractDir);
			trace = String.format("    %s deleted.", extractDir.getAbsolutePath());
			log.add(trace);
			Util.LOGGER.info(trace);
		}
		FileUtil.extractZipArchive(backup, extractDir);
		trace = String.format("Extracted %s to %s", backup.getAbsolutePath(), extractDir.getAbsolutePath());
		log.add(trace);
		Util.LOGGER.info(trace);
		setPercentComplete(50);

		PreProcess restorePreProcess = options.getPreProcess();
		ContentOption contrentRestoreOption =  options.getContentOption();

		boolean truncateDatabase = PreProcess.deleteData.equals(restorePreProcess);
		if (truncateDatabase) {
			trace = "Truncate " + ((UtilImpl.SCHEMA == null) ? "default" : UtilImpl.SCHEMA) + " schema";
			log.add(trace);
			Util.LOGGER.info(trace);
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
		Util.LOGGER.info(trace);
		IndexingOption indexingOption = options.getIndexingOption();
		restore(extractDirName, createUsingBackup, contrentRestoreOption, indexingOption);
		if (ddlSync) {
			trace = "DDL Sync";
			log.add(trace);
			Util.LOGGER.info(trace);
			DDL.sync(true);
		}
		if (IndexingOption.both.equals(indexingOption) || IndexingOption.data.equals(indexingOption)) {
			trace = "Reindex textual indexes.";
			log.add(trace);
			Util.LOGGER.info(trace);
			execute(new ReindexBeansJob());
		}
		trace = "Delete extracted folder " + extractDir.getAbsolutePath();
		log.add(trace);
		Util.LOGGER.info(trace);
		FileUtil.delete(extractDir);
		trace = "DONE";
		log.add(trace);
		Util.LOGGER.info(trace);
		setPercentComplete(100);

		EXT.push(new PushMessage().growl(MessageSeverity.info, "System Restore complete."));
	}

	private void restore(String extractDirName,
							boolean createUsingBackup,
							ContentOption contentRestoreOption,
							IndexingOption indexingOption)
	throws Exception {
		String customerName = CORE.getUser().getCustomerName();

		String backupDirectoryPath = UtilImpl.CONTENT_DIRECTORY +
										"backup_" + customerName +
										File.separator + extractDirName;
		File backupDirectory = new File(backupDirectoryPath);
		if ((! backupDirectory.exists()) || (! backupDirectory.isDirectory())) {
			throw new IllegalArgumentException(backupDirectoryPath + " is not a directory");
		}

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
				String trace = "    restore table " + table.name;
				log.add(trace);
				UtilImpl.LOGGER.info(trace);
				File backupFile = new File(backupDirectory.getAbsolutePath() + File.separator + table.name + ".csv");
				if (! backupFile.exists()) {
					trace = "        ***** File " + backupFile.getAbsolutePath() + File.separator + table.name + ".csv does not exist";
					log.add(trace);
					System.err.println(trace);
					continue;
				}

				long rowCount = 0;

				try (InputStreamReader in = new InputStreamReader(new FileInputStream(backupFile), StandardCharsets.UTF_8)) {
					try (CsvMapReader reader = new CsvMapReader(in, CsvPreference.STANDARD_PREFERENCE)) {
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
									else if (AttributeType.content.equals(attributeType) ||
												AttributeType.image.equals(attributeType)) {
										StringBuilder contentPath = new StringBuilder(128);
										contentPath.append(backupDirectory.getAbsolutePath()).append('/');
										contentPath.append(AbstractContentManager.FILE_STORE_NAME).append('/');

										AttachmentContent content = ElasticContentManager.getFromFileSystem(contentPath, stringValue);
										if (content == null) {
											trace = "        Could not find file associated with " + stringValue;
											if (ContentOption.error.equals(contentRestoreOption)) {
												log.add(trace);
												Util.LOGGER.severe(trace);
												throw new DomainException(trace);
											}
											else if (ContentOption.clearOrphanedContentIds.equals(contentRestoreOption)) {
												trace += " : Setting content to null";
												log.add(trace);
												Util.LOGGER.info(trace);
												statement.setString(index++, null);
											}
											else {
												trace += " : Setting content ID regardless";
												log.add(trace);
												Util.LOGGER.info(trace);
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
										throw new IllegalStateException("No value set for " + header);
									}
								} // for (each header)

								statement.executeUpdate();
								rowCount++;
							} // while (each CSV line)

							connection.commit();
						}
						catch (Throwable t) {
							trace = t.getLocalizedMessage();
							log.add(trace);
							Util.LOGGER.severe(trace);
							trace = "AT LINE " + rowCount + " OF " + backupFile.getAbsolutePath();
							log.add(trace);
							Util.LOGGER.severe(trace);
							trace = "CAUSED BY:- " + sql.toString();
							log.add(trace);
							Util.LOGGER.severe(trace);


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
							Util.LOGGER.severe(trace);

							throw t;
						}
					}
				}
				trace = "    restored table " + table.name + " with " + rowCount + " rows.";
				log.add(trace);
				UtilImpl.LOGGER.info(trace);
			} // for (each table)
		}
	}

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
			trace = "    restore foreign keys for table " + table.name;
			log.add(trace);
			Util.LOGGER.info(trace);
			File backupFile = new File(backupDirectory.getAbsolutePath() + File.separator + table.name + ".csv");
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
							} // while (each CSV line)

							connection.commit();
						}
					}
				}
			}
			trace = "    restored foreign keys for table " + table.name + " with " + rowCount + " rows.";
			log.add(trace);
			UtilImpl.LOGGER.info(trace);
		} // for (each table)
	}
}
