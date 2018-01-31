package org.skyve.impl.backup;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.util.Collection;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.impl.backup.BackupUtil;
import org.skyve.impl.backup.JoinTable;
import org.skyve.impl.backup.Table;
import org.supercsv.io.CsvMapReader;
import org.supercsv.prefs.CsvPreference;

public class RestoreFromJackrabbit {
	private static void restore() 
	throws Exception {
		String customerName = CORE.getUser().getCustomerName();
		String backupDirectoryPath = "./backup_" + customerName;
		File backupDirectory = new File(backupDirectoryPath);
		if ((! backupDirectory.exists()) || (! backupDirectory.isDirectory())) {
			throw new IllegalArgumentException(backupDirectoryPath + " is not a directory");
		}

		Collection<Table> tables = BackupUtil.getTables();
		restoreContent(backupDirectory, tables);
	}

	private static void restoreContent(File backupDirectory,
										Collection<Table> tables) 
	throws Exception {
		AbstractHibernatePersistence persistence = (AbstractHibernatePersistence) AbstractPersistence.get();
		try {
			persistence.begin();
			try (ContentManager cm = EXT.newContentManager()) {
				for (Table table : tables) {
					if (table instanceof JoinTable) {
						continue;
					}
					UtilImpl.LOGGER.info("restore table " + table.name);
					File backupFile = new File(backupDirectory.getAbsolutePath() + File.separator + table.name + ".csv");
					if (! backupFile.exists()) {
						System.err.println("***** File " + backupFile.getAbsolutePath() + File.separator + " does not exist");
						continue;
					}
					try (FileReader fr = new FileReader(backupFile)) {
						try (CsvMapReader reader = new CsvMapReader(fr, CsvPreference.STANDARD_PREFERENCE)) {
							String[] headers = reader.getHeader(true);
	
							Map<String, String> values = null;
							while ((values = reader.read(headers)) != null) {
								for (String header : headers) {
									if (header.endsWith("_id")) {
										continue;
									}
									String stringValue = values.get(header);
									if ((stringValue == null) || (stringValue.length() == 0)) {
										continue;
									}

									AttributeType attributeType = table.fields.get(header);

									// replace any 2 CR or LF combinations in the string with 1
									// Super CSV place 2 ox0A into the string when it comes across a '\n'
									// in a quoted string field value.
									stringValue = stringValue.replaceAll("[\\n\\r]{2}", "\n");
									if (attributeType == AttributeType.content) {
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
																				stringValue.substring(4, 6) + File.separator);
System.out.println(candidateDirectory.getAbsolutePath());
											File[] files = candidateDirectory.listFiles();
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
										}
										else {
											String dataGroupId = values.get(Bean.DATA_GROUP_ID);
											if (dataGroupId.isEmpty()) {
												dataGroupId = null;
											}
											try (BufferedInputStream stream = new BufferedInputStream(new FileInputStream(contentFile))) {
												MimeType mimeType = MimeType.fromFileName(contentFile.getName());
												AttachmentContent content = new AttachmentContent(values.get(Bean.CUSTOMER_NAME), 
																									moduleName,
																									documentName, 
																									dataGroupId, 
																									values.get(Bean.USER_ID), 
																									values.get(Bean.DOCUMENT_ID),
																									header,
																									documentName + '.' + mimeType.getStandardFileSuffix(),
																									mimeType,
																									stream);
												content.setContentId(fileName);
												cm.put(content);
											}
											catch (Exception e) {
												System.err.println("Error encountered trying to put file " + contentFile.getAbsolutePath() + " into content management");
												e.printStackTrace();
											}
										}
									}
								} // for (each header)
							} // while (each CSV line)
						}
					}
				} // for (each table)
			}
		}
		finally {
			persistence.commit(false);
		}
	}

	public static void main(String[] args) throws Exception {
		if (args.length != 8) {
			System.err.println("args are <customerName> <content directory> <content file storage?> <DB dialect> <DB driver> <DB URL> <DB username> <DB password>");
			System.exit(1);
		}

		BackupUtil.initialise(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
		try {
			restore();
		}
		finally {
			BackupUtil.finalise();
			
			// This is required to stop the process hanging at the end
			System.exit(0);
		}
	}
}
