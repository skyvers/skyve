package org.skyve.impl.backup;

import java.io.File;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

public class ContentChecker {
	@SuppressWarnings("static-method")
	public void checkContent() throws Exception {
		Customer customer = CORE.getCustomer();
		String customerName = customer.getName();

		try (Connection connection = EXT.getDataStoreConnection()) {
			connection.setAutoCommit(false);

			try (ContentManager cm = EXT.newContentManager()) {
				int missingContentCount = 0;
				int erroneousContentCount = 0;

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
							UtilImpl.LOGGER.info("Checking content for " + table.name);

							while (resultSet.next()) {
								for (String name : table.fields.keySet()) {
									AttributeType attributeType = table.fields.get(name);

									if (AttributeType.content.equals(attributeType) || AttributeType.image.equals(attributeType)) {
										String stringValue = resultSet.getString(name);
										if (! resultSet.wasNull()) {
											AttachmentContent content;
											try {
												content = cm.getAttachment(stringValue);
												if (content == null) {
													UtilImpl.LOGGER.severe("Detected missing content " + stringValue + " for field name " + name + " for table " + table.name);

													// Construct what would be the file path.
													final File contentDirectory = Paths.get(UtilImpl.CONTENT_DIRECTORY, ContentManager.FILE_STORE_NAME).toFile();
													final StringBuilder contentAbsolutePath = new StringBuilder(contentDirectory.getAbsolutePath() + File.separator);
													AbstractContentManager.appendBalancedFolderPathFromContentId(stringValue, contentAbsolutePath, true);
													final File contentFile = Paths.get(contentAbsolutePath.toString(), stringValue).toFile();

													if (contentFile.exists()) {
														UtilImpl.LOGGER.severe("Found matching file for missing content " + contentFile.getAbsolutePath());
													}
													missingContentCount++;
												}
												else {
													String attributeName = content.getAttributeName();
													if (! name.equals(attributeName)) {
														UtilImpl.LOGGER.severe("Detected error in content " + stringValue + " for field name " + name + " for table " + table.name + ": Content Attribute Name " + attributeName + " does not match content field name " + name);
														erroneousContentCount++;
													}
													else {
														String bizModule = content.getBizModule();
														String bizDocument = content.getBizDocument();
														try {
															Module m = customer.getModule(bizModule);
															Document d = m.getDocument(customer, bizDocument);
															Persistent p = d.getPersistent();
															if (p == null) {
																UtilImpl.LOGGER.severe("Detected error in content " + stringValue + " for field name " + name + " for table " + table.name + ": Content Document " + bizModule + "." + bizDocument + " is not persistent");
																erroneousContentCount++;
															}
															else if (! table.name.equals(p.getPersistentIdentifier())) {
																UtilImpl.LOGGER.severe("Detected error in content " + stringValue + " for field name " + name + " for table " + table.name + ": Content Document " + bizModule + "." + bizDocument + " has a persistent identifier of " + p.getPersistentIdentifier());
																erroneousContentCount++;
															}
															else {
																// Find the attribute in the document hierarchy
																Attribute a = d.getAttribute(attributeName);
																Document base = d;
																Extends extension = d.getExtends();
																while ((a == null) && (extension != null)) {
																	Module basemod = customer.getModule(base.getOwningModuleName());
																	base = basemod.getDocument(customer, extension.getDocumentName());
																	a = base.getAttribute(attributeName);
																	extension = base.getExtends();
																}

																if (a == null) {
																	UtilImpl.LOGGER.severe("Detected error in content " + stringValue + " for field name " + name + " for table " + table.name + ": Content Attribute Name " + attributeName + " does not exist for document " + bizModule + "." + bizDocument);
																	erroneousContentCount++;
																}
																else {
																	AttributeType type = a.getAttributeType();
																	if (type != attributeType) {
																		UtilImpl.LOGGER.severe("Detected error in content " + stringValue + " for field name " + name + " for table " + table.name + ": Content Attribute Name " + name + " is not a(n) " + attributeType + " for document " + bizModule + "." + bizDocument);
																		erroneousContentCount++;
																	}
																}
															}
														}
														catch (@SuppressWarnings("unused") Exception e) {
															UtilImpl.LOGGER.severe("Detected error in content " + stringValue + " for field name " + name + " for table " + table.name + ": Content Document " + bizModule + "." + bizDocument + " does not exist for customer " + customerName);
															erroneousContentCount++;
														}
													}
												}
											}
											catch (Exception e) {
												UtilImpl.LOGGER.severe("Error checking content " + stringValue + " for field name " + name + " for table " + table.name);
												e.printStackTrace();
											}
										}
									}
								}
							}
						}
					}
					catch (SQLException e) {
						UtilImpl.LOGGER.severe(sql.toString());
						throw e;
					}
				}
				connection.commit();
				UtilImpl.LOGGER.info("MISSING CONTENT COUNT = " + missingContentCount);
				UtilImpl.LOGGER.info("ERRONEOUS CONTENT COUNT = " + erroneousContentCount);
			}
		}
	}

	private static boolean hasContent(Table table) {
		for (String name : table.fields.keySet()) {
			AttributeType attributeType = table.fields.get(name);
			if (AttributeType.content.equals(attributeType) || AttributeType.image.equals(attributeType)) {
				return true;
			}
		}
		return false;
	}
}
