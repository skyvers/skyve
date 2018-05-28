package org.skyve.impl.backup;

import org.apache.tika.Tika;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.Attribute;
import org.skyve.util.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

public class ContentChecker {
    private static final Logger LOGGER = LoggerFactory.getLogger(ContentChecker.class);

    @SuppressWarnings("static-method")
	public void checkContent() throws Exception {
        String customerName = CORE.getUser().getCustomerName();

        try (Connection connection = EXT.getDataStoreConnection()) {
            connection.setAutoCommit(false);

            try (ContentManager cm = EXT.newContentManager()) {
                for (Table table : BackupUtil.getTables()) {
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
            }

        }
    }
    
	@SuppressWarnings("static-method")
	public void relinkContent() throws Exception {
		Tika tika = new Tika();
		
		String customerName = CORE.getUser().getCustomerName();

		try (Connection connection = EXT.getDataStoreConnection()) {
			connection.setAutoCommit(false);

			try (ContentManager cm = EXT.newContentManager()) {
				for (Table table : BackupUtil.getTables()) {
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
													LOGGER.error("Detected missing content {} for field name {} for table {}",
																	stringValue, name, table.name);

													// Construct what would be the file path.
													final File contentDirectory = Paths.get(UtilImpl.CONTENT_DIRECTORY, AbstractContentManager.FILE_STORE_NAME).toFile();
													final StringBuilder contentAbsolutePath = new StringBuilder(contentDirectory.getAbsolutePath() + File.separator);
													AbstractContentManager.appendBalancedFolderPathFromContentId(stringValue, contentAbsolutePath, true);
													final File contentFile = Paths.get(contentAbsolutePath.toString(), stringValue).toFile();
													
													if (contentFile.exists()) {
														String contentType = Files.probeContentType(contentFile.toPath());
														if (contentType == null) {
															contentType = tika.detect(contentFile);
														}
														LOGGER.error("Found matching file for missing content {} of type {}. Relink", contentFile.getAbsolutePath(), contentType);
														MimeType mimeType = MimeType.fromMimeType(contentType);
														content = new AttachmentContent(resultSet.getString(Bean.CUSTOMER_NAME),
																							resultSet.getString(Bean.MODULE_KEY),
																							resultSet.getString(Bean.DOCUMENT_KEY),
																							resultSet.getString(Bean.DATA_GROUP_ID),
																							resultSet.getString(Bean.USER_ID),
																							resultSet.getString(Bean.DOCUMENT_ID),
																							name,
																							"content." + mimeType.getStandardFileSuffix(),
																							mimeType,
																							contentFile);
														cm.put(content);
													}
												}
											}
											catch (Exception e) {
												LOGGER.error("Error checking content {} for field name {} for table {}",
																stringValue, name, table.name, e);
											}
										}
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
