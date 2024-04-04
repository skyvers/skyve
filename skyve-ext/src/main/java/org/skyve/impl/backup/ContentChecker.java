package org.skyve.impl.backup;

import java.io.File;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.RDBMSDynamicPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.util.JSON;

public class ContentChecker {
	private int missingContentCount = 0;
	private int erroneousContentCount = 0;
	
	public void checkContent() throws Exception {
		Customer customer = CORE.getCustomer();
		String customerName = customer.getName();

		try (Connection connection = EXT.getDataStoreConnection()) {
			connection.setAutoCommit(false);

			try (ContentManager cm = EXT.newContentManager()) {
				missingContentCount = 0;
				erroneousContentCount = 0;

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
									AttributeType attributeType = table.fields.get(name).getLeft();
									if (AttributeType.content.equals(attributeType) || AttributeType.image.equals(attributeType)) {
										String stringValue = resultSet.getString(name);
										if (! resultSet.wasNull()) {
											checkContent(stringValue, cm, name, table.name, attributeType, customer);
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
				
				// Check dynamic documents
				try (Statement statement = connection.createStatement()) {
					// Iterate through all DynamicEntities looking for content/image attribute values
					StringBuilder sql = new StringBuilder(128);
					sql.append("select bizId, moduleName, documentName, fields from ").append(RDBMSDynamicPersistence.DYNAMIC_ENTITY_TABLE_NAME);
					if (UtilImpl.CUSTOMER == null) {
						sql.append(" where ").append(Bean.CUSTOMER_NAME).append(" = '").append(customerName).append('\'');
					}
					statement.execute(sql.toString());
					try (ResultSet resultSet = statement.getResultSet()) {
						UtilImpl.LOGGER.info("Checking dynamic domain for content");

						while (resultSet.next()) {
							// Get the document for the dynamic row
							Module m = customer.getModule(resultSet.getString(2));
							Document d = m.getDocument(customer, resultSet.getString(3));
							Map<String, Object> fieldsJSON = null;
							for (Attribute a : d.getAllAttributes(customer)) {
								AttributeType t = a.getAttributeType();
								if ((t == AttributeType.content) || (t == AttributeType.image)) {
									if (fieldsJSON == null) {
										@SuppressWarnings("unchecked")
										Map<String, Object> json = (Map<String, Object>) JSON.unmarshall(null, resultSet.getString(4));
										fieldsJSON = json;
									}
									String fieldName = a.getName();
									String contentId = (String) fieldsJSON.get(fieldName);
									if (contentId != null) {
										checkContent(contentId, cm, fieldName, RDBMSDynamicPersistence.DYNAMIC_ENTITY_TABLE_NAME, t, customer);
									}
								}
							}
						}
					}
				}

				connection.commit();
				UtilImpl.LOGGER.info("MISSING CONTENT COUNT = " + missingContentCount);
				UtilImpl.LOGGER.info("ERRONEOUS CONTENT COUNT = " + erroneousContentCount);
			}
		}
	}

	private void checkContent(String contentId,
								ContentManager cm,
								String fieldName,
								String tableName,
								AttributeType attributeType,
								Customer customer) {
		AttachmentContent content;
		try {
			content = cm.getAttachment(contentId);
			if (content == null) {
				UtilImpl.LOGGER.severe("Detected missing content " + contentId + " for field name " + fieldName + " for table " + tableName);

				// Construct what would be the file path.
				final File contentDirectory = Paths.get(UtilImpl.CONTENT_DIRECTORY, ContentManager.FILE_STORE_NAME).toFile();
				final StringBuilder contentAbsolutePath = new StringBuilder(contentDirectory.getAbsolutePath() + File.separator);
				AbstractContentManager.appendBalancedFolderPathFromContentId(contentId, contentAbsolutePath, true);
				final File contentFile = Paths.get(contentAbsolutePath.toString(), contentId).toFile();

				if (contentFile.exists()) {
					UtilImpl.LOGGER.severe("Found matching file for missing content " + contentFile.getAbsolutePath());
				}
				missingContentCount++;
			}
			else {
				String attributeName = content.getAttributeName();
				String noEmbeddedPrefixFieldName = fieldName;
				int embeddedPrefixHyphenIndex = fieldName.lastIndexOf('_');
				if (embeddedPrefixHyphenIndex >= 0) {
					noEmbeddedPrefixFieldName = fieldName.substring(embeddedPrefixHyphenIndex + 1);
				}
				if (! noEmbeddedPrefixFieldName.equals(attributeName)) {
					UtilImpl.LOGGER.severe("Detected error in content " + contentId + " for field name " + fieldName + " for table " + tableName + ": Content Attribute Name " + attributeName + " does not match content field name " + noEmbeddedPrefixFieldName);
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
							UtilImpl.LOGGER.severe("Detected error in content " + contentId + " for field name " + fieldName + " for table " + tableName + ": Content Document " + bizModule + "." + bizDocument + " is not persistent");
							erroneousContentCount++;
						}
						else if ((embeddedPrefixHyphenIndex < 0) && // not content through an embedded association
									(! tableName.equals(RDBMSDynamicPersistence.DYNAMIC_ENTITY_TABLE_NAME)) && // not a dynamic entity
									(! tableName.equals(p.getPersistentIdentifier()))) {
							UtilImpl.LOGGER.severe("Detected error in content " + contentId + " for field name " + fieldName + " for table " + tableName + ": Content Document " + bizModule + "." + bizDocument + " has a persistent identifier of " + p.getPersistentIdentifier());
							erroneousContentCount++;
						}
						else {
							Attribute a = d.getPolymorphicAttribute(customer, attributeName);

							if (a == null) {
								UtilImpl.LOGGER.severe("Detected error in content " + contentId + " for field name " + fieldName + " for table " + tableName + ": Content Attribute Name " + attributeName + " does not exist for document " + bizModule + "." + bizDocument);
								erroneousContentCount++;
							}
							else {
								AttributeType type = a.getAttributeType();
								if (type != attributeType) {
									UtilImpl.LOGGER.severe("Detected error in content " + contentId + " for field name " + fieldName + " for table " + tableName + ": Content Attribute Name " + fieldName + " is not a(n) " + attributeType + " for document " + bizModule + "." + bizDocument);
									erroneousContentCount++;
								}
							}
						}
					}
					catch (@SuppressWarnings("unused") Exception e) {
						UtilImpl.LOGGER.severe("Detected error in content " + contentId + " for field name " + fieldName + " for table " + tableName + ": Content Document " + bizModule + "." + bizDocument + " does not exist for customer " + customer.getName());
						erroneousContentCount++;
					}
				}
			}
		}
		catch (Exception e) {
			UtilImpl.LOGGER.severe("Error checking content " + contentId + " for field name " + fieldName + " for table " + tableName);
			e.printStackTrace();
		}
	}
	
	private Collection<Table> tablesForAllCustomers = null;
	
	public String bogusContentReference(String contentId) throws Exception {
		if (tablesForAllCustomers == null) {
			tablesForAllCustomers = BackupUtil.getTablesForAllCustomers();
		}
		
		Persistence p = CORE.getPersistence();
		StringBuilder sql = new StringBuilder(128);
		
		for (Table table : tablesForAllCustomers) {
			sql.setLength(0);

			for (String name : table.fields.keySet()) {
				AttributeType attributeType = table.fields.get(name).getLeft();
				if (AttributeType.content.equals(attributeType) || AttributeType.image.equals(attributeType)) {
					if (sql.length() == 0) {
						sql.append("select bizId from ").append(table.name).append(" where ");
					}
					else {
						sql.append(" or ");
					}
					sql.append(name).append(" = :contentId");
				}
			}

			if (sql.length() > 0) {
				String rowBizId = p.newSQL(sql.toString()).putParameter("contentId", contentId, false).scalarResult(String.class);
				if (rowBizId != null) {
					return table.name + '#' + rowBizId;
				}
			}
		}
		
		// Check dynamic documents
		ProvidedRepository r = ProvidedRepositoryFactory.get();
		// Find any DynamicEntity with the given contentId as a value in the fields JSON
		StringBuilder like = new StringBuilder(64).append("%\":\"").append(contentId).append("\"%");
		SQL q = p.newSQL("select bizId, bizCustomer, moduleName, documentName, fields from " + RDBMSDynamicPersistence.DYNAMIC_ENTITY_TABLE_NAME + " where fields like :like");
		List<Object[]> rows = q.putParameter("like", like.toString(), false).tupleResults();
		for (Object[] row : rows) {
			// Determine the attribute name from the fields JSON, 
			// and if the attribute exists and is of type content or image, 
			// we have a problem as the content is linked by a dynamic document.
			String fields = (String) row[4];
			int valueIndex = fields.indexOf(like.substring(1, like.length() - 1)); // remove % from either end of like
			fields = fields.substring(0, valueIndex);
			int previousDoubleQuoteIndex = fields.lastIndexOf('"');
			if (previousDoubleQuoteIndex >= 0) {
				String attributeName = fields.substring(previousDoubleQuoteIndex + 1);
				Customer c = r.getCustomer((String) row[1]);
				Module m = r.getModule(c, (String) row[2]);
				Document d = m.getDocument(c, (String) row[3]);
				Attribute a = d.getPolymorphicAttribute(c, attributeName);
				if (a != null) {
					AttributeType t = a.getAttributeType();
					if ((t == AttributeType.content) || (t == AttributeType.image)) {
						return RDBMSDynamicPersistence.DYNAMIC_ENTITY_TABLE_NAME + '#' + row[0];
					}
				}
			}
		}

		return null;
	}
	
	private static boolean hasContent(Table table) {
		for (String name : table.fields.keySet()) {
			AttributeType attributeType = table.fields.get(name).getLeft();
			if (AttributeType.content.equals(attributeType) || AttributeType.image.equals(attributeType)) {
				return true;
			}
		}
		return false;
	}
}
