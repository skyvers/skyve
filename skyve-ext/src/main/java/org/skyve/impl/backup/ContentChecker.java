package org.skyve.impl.backup;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.tika.Tika;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.elastic.ESClient;
import org.skyve.impl.content.elastic.ElasticContentManager;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.util.FileUtil;
import org.skyve.util.JSON;
import org.skyve.util.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ContentChecker {
    private static final Logger LOGGER = LoggerFactory.getLogger(ContentChecker.class);

    @SuppressWarnings("static-method")
	public void checkContent() throws Exception {
        String customerName = CORE.getUser().getCustomerName();

        try (Connection connection = EXT.getDataStoreConnection()) {
            connection.setAutoCommit(false);

            try (ContentManager cm = EXT.newContentManager()) {
            	int missingContentCount = 0;
            	
            	for (Table table : BackupUtil.getTables()) {
                	if (! hasContent(table)) {
                		continue;
                	}
                	
                    StringBuilder sql = new StringBuilder(128);
                    try (Statement statement = connection.createStatement()) {
                        sql.append("select * from ").append(table.name);
                        BackupUtil.secureSQL(sql, table, customerName);
                        statement.execute(sql.toString());
                        try (ResultSet resultSet = statement.getResultSet()) {
                            LOGGER.info("Checking content for " + table.name);

                            while (resultSet.next()) {
                                for (String name : table.fields.keySet()) {
                                    Attribute.AttributeType attributeType = table.fields.get(name);

                                    if (Attribute.AttributeType.content.equals(attributeType)) {
                                        String stringValue = resultSet.getString(name);
                                        if (! resultSet.wasNull()) {
                                            AttachmentContent content;
                                            try {
                                                content = cm.get(stringValue);
                                                if (content == null) {
                                                    LOGGER.error("Detected missing content {} for field name {} for table {}", stringValue, name, table.name);

                                                    // Construct what would be the file path.
                                                    final File contentDirectory = Paths.get(UtilImpl.CONTENT_DIRECTORY, AbstractContentManager.FILE_STORE_NAME).toFile();
                                                    final StringBuilder contentAbsolutePath = new StringBuilder(contentDirectory.getAbsolutePath() + File.separator);
                                                    AbstractContentManager.appendBalancedFolderPathFromContentId(stringValue, contentAbsolutePath, true);
                                                    final File contentFile = Paths.get(contentAbsolutePath.toString(), stringValue).toFile();

                                                    if (contentFile.exists()) {
                                                        LOGGER.error("Found matching file for missing content {}.", contentFile.getAbsolutePath());
                                                    }
                                                    missingContentCount++;
                                                }
                                            } catch (Exception e) {
                                                LOGGER.error("Error checking content {} for field name {} for table {}", stringValue, name, table.name, e);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } catch (SQLException e) {
                        Util.LOGGER.severe(sql.toString());
                        throw e;
                    }
                }
                connection.commit();
                LOGGER.info("MISSING CONTENT COUNT = " + missingContentCount);
            }
        }
    }
    
    @Deprecated
	@SuppressWarnings("static-method")
	public void migrateContent() throws Exception {
		StringBuilder path = new StringBuilder(128);
		path.append(UtilImpl.CONTENT_DIRECTORY).append(AbstractContentManager.FILE_STORE_NAME).append("_BACKUP/");
		File backupFolder = new File(path.toString());
		if (! backupFolder.exists()) {
			throw new ValidationException(new Message("No backup file content folder at " + backupFolder.getAbsolutePath()));
		}

		Tika tika = new Tika();
		
		String customerName = CORE.getUser().getCustomerName();

		try (Connection connection = EXT.getDataStoreConnection()) {
			connection.setAutoCommit(false);

			try (ContentManager ocm = new ESClient(true)) {
				try (ContentManager cm = EXT.newContentManager()) {
					for (Table table : BackupUtil.getTables()) {
	                	if (! hasContent(table)) {
	                		continue;
	                	}

	                	StringBuilder sql = new StringBuilder(128);
						try (Statement statement = connection.createStatement()) {
							sql.append("select * from ").append(table.name);
							BackupUtil.secureSQL(sql, table, customerName);
							statement.execute(sql.toString());
							try (ResultSet resultSet = statement.getResultSet()) {
								LOGGER.info("Migrating content for " + table.name);
	
								while (resultSet.next()) {
									for (String name : table.fields.keySet()) {
										Attribute.AttributeType attributeType = table.fields.get(name);
	
										if (Attribute.AttributeType.content.equals(attributeType)) {
											String stringValue = resultSet.getString(name);
											if (! resultSet.wasNull()) {
												AttachmentContent content;
												try {
													// Get the content using the old content manager
													content = ocm.get(stringValue);
													if (content == null) {
														LOGGER.error("Missing content {} for field name {} for table {}",
																		stringValue, name, table.name);
													}
													else {
														if (content.getContentBytes() == null) {
															LOGGER.error("Missing content bytes {} for field name {} for table {}",
																			stringValue, name, table.name);
															content = null;
														}
													}
													if (content == null) {
														path.setLength(0);
														path.append(UtilImpl.CONTENT_DIRECTORY).append(AbstractContentManager.FILE_STORE_NAME).append("_BACKUP/");
														AbstractContentManager.appendBalancedFolderPathFromContentId(stringValue, path, true);
														path.append(stringValue);
														File contentFile = new File(path.toString());
														if (contentFile.exists()) {
															String contentType = Files.probeContentType(contentFile.toPath());
															if (contentType == null) {
																contentType = tika.detect(contentFile);
															}
															LOGGER.info("Found matching file for missing content {} of type {}. Relink", contentFile.getAbsolutePath(), contentType);
															MimeType mimeType = MimeType.fromContentType(contentType);
															Document document = findDocumentForPersistentName(table.name);
															content = new AttachmentContent(resultSet.getString(Bean.CUSTOMER_NAME),
																								document.getOwningModuleName(),
																								document.getName(),
																								resultSet.getString(Bean.DATA_GROUP_ID),
																								resultSet.getString(Bean.USER_ID),
																								resultSet.getString(Bean.DOCUMENT_ID),
																								name,
																								"content." + mimeType.getStandardFileSuffix(),
																								mimeType,
																								contentFile);
														}
														else {
															LOGGER.error("Error migrating content {} - No matching file for missing content {}",
																			stringValue, contentFile.getAbsolutePath());
														}
													}
													if (content != null) {
														content.setContentId(null); // put new
														cm.put(content, false);
														
														String update = String.format("update %s set %s = '%s' where bizId = '%s'",
																						table.name,
																						name,
																						content.getContentId(),
																						content.getBizId());
														try (Statement s = connection.createStatement()) {
															s.execute(update);
														}
													}
												}
												catch (Exception e) {
													LOGGER.error("Error migrating content {} for field name {} for table {}",
																	stringValue, name, table.name, e);
												}
											}
										}
									}
								}
							}
							connection.commit();
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

    @Deprecated
	public static void migrateContentFiles()
	throws Exception {
		File storeDir = new File(UtilImpl.CONTENT_DIRECTORY + AbstractContentManager.FILE_STORE_NAME);
		for (File firstDir : storeDir.listFiles()) {
			if (firstDir.isDirectory()) {
				for (File secondDir : firstDir.listFiles()) {
					if (secondDir.isDirectory()) {
						for (File thirdDir : secondDir.listFiles()) {
							if (thirdDir.isDirectory()) {
								for (File fourthDir : thirdDir.listFiles()) {
									if (fourthDir.isDirectory()) {
										for (File file : fourthDir.listFiles()) {
											String fileName = file.getName();
											// rewrite JSON with the correct content type
											if ("meta.json".equals(fileName)) {
												@SuppressWarnings("unchecked")
												Map<String, Object> meta = (Map<String, Object>) JSON.unmarshall(null, FileUtil.getFileAsString(file));
												String contentType = (String) meta.get("content_type");
												if (MimeType.plain.toString().equals(contentType)) {
													MimeType mimeType = MimeType.fromFileName((String) meta.get("filename"));
													meta.put("content_type", (mimeType == null) ? null : mimeType.toString());
													try (FileWriter fw = new FileWriter(file)) {
														fw.write(JSON.marshall(null, meta, null));
														fw.flush();
													}
												}
											}
											else if (! fileName.endsWith("_old")) {
												File newFile = new File(fourthDir, ElasticContentManager.CONTENT);
												if (Files.move(file.toPath(), newFile.toPath(), StandardCopyOption.REPLACE_EXISTING) == null) {
													throw new IOException("Could not rename " + file + " to " + newFile);
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	private static boolean hasContent(Table table) {
		for (String name : table.fields.keySet()) {
			AttributeType attributeType = table.fields.get(name);
			if (AttributeType.content.equals(attributeType)) {
				return true;
			}
		}
		return false;
	}
	
	@Deprecated
	private static Document findDocumentForPersistentName(String tableName) {
		Customer c = CORE.getUser().getCustomer();
		for (Module m : c.getModules()) {
			for (Entry<String, DocumentRef> entry : m.getDocumentRefs().entrySet()) {
				DocumentRef documentRef = entry.getValue();
				if (documentRef.getOwningModuleName().equals(m.getName())) {
					Document d = m.getDocument(c, entry.getKey());
					Persistent p = d.getPersistent();
					if ((p != null) && tableName.equals(p.getName())) {
						return d;
					}
				}
			}
		}
		return null;
	}
}
