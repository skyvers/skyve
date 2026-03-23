package org.skyve.impl.backup;

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
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.model.list.RDBMSDynamicPersistenceListModel;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.util.JSON;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class ContentChecker {
    private static final Logger LOGGER = LoggerFactory.getLogger(ContentChecker.class);

	private int missingContentCount = 0;
	private int erroneousContentCount = 0;
	
    /**
     * Check the content repository for missing or inconsistent content references.
     */
	public void checkContent() throws Exception {
		Customer customer = CORE.getCustomer();
		String customerName = customer.getName();
		String dynamicEntityPersistentIdentifier = RDBMSDynamicPersistenceListModel.getDynamicEntityPersistent(customer).getPersistentIdentifier();
		
		try (Connection connection = EXT.getDataStoreConnection()) {
			connection.setAutoCommit(false);

			try (ContentManager cm = EXT.newContentManager()) {
				missingContentCount = 0;
				erroneousContentCount = 0;
				
				checkStaticContent(connection, cm, customer, customerName);
				checkDynamicContent(connection, cm, customer, customerName, dynamicEntityPersistentIdentifier);

				connection.commit();
				if (LOGGER.isInfoEnabled()) {
					LOGGER.info("MISSING CONTENT COUNT = {}", String.valueOf(missingContentCount));
					LOGGER.info("ERRONEOUS CONTENT COUNT = {}", String.valueOf(erroneousContentCount));
				}
			}
		}
	}
	
	/**
	 * Check static (non-dynamic) tables for content references.
	 *
	 * @param connection database connection
	 * @param cm content manager
	 * @param customer customer to validate against
	 * @param customerName customer name for filtering
	 * @throws Exception if validation fails
	 */
	@SuppressWarnings("java:S3776") // complexity OK
	private void checkStaticContent(Connection connection, ContentManager cm, Customer customer, String customerName) throws Exception {
		for (Table table : BackupUtil.getTables()) {
			if (! hasContent(table)) {
				continue;
			}

			StringBuilder sql = new StringBuilder(128);
			try (Statement statement = connection.createStatement()) {
				sql.append("select * from ").append(table.persistentIdentifier);
				BackupUtil.secureSQL(sql, table, customerName);
				statement.execute(sql.toString());
				try (ResultSet resultSet = statement.getResultSet()) {
					LOGGER.info("Checking content for {}", table.persistentIdentifier);

					while (resultSet.next()) {
						for (String name : table.fields.keySet()) {
							AttributeType attributeType = table.fields.get(name).getAttributeType();
							if (AttributeType.content.equals(attributeType) || AttributeType.image.equals(attributeType)) {
								String stringValue = resultSet.getString(name);
								if (! resultSet.wasNull()) {
									checkContent(stringValue, cm, name, table.persistentIdentifier, attributeType, customer, false);
								}
							}
						}
					}
				}
			}
			catch (SQLException e) {
				LOGGER.error(sql.toString());
				throw e;
			}
		}
	}
	
	/**
	 * Check dynamic entities for content references.
	 *
	 * @param connection database connection
	 * @param cm content manager
	 * @param customer customer to validate against
	 * @param customerName customer name for filtering
	 * @param dynamicEntityPersistentIdentifier persistent identifier for DynamicEntity
	 * @throws Exception if validation fails
	 */
	@SuppressWarnings("java:S3776") // complexity OK
	private void checkDynamicContent(Connection connection,
										ContentManager cm,
										Customer customer,
										String customerName,
										String dynamicEntityPersistentIdentifier)
	throws Exception {
		try (Statement statement = connection.createStatement()) {
			// Iterate through all DynamicEntities looking for content/image attribute values
			StringBuilder sql = new StringBuilder(128);
			sql.append("select bizId, moduleName, documentName, fields from ").append(dynamicEntityPersistentIdentifier);
			if (UtilImpl.CUSTOMER == null) {
				sql.append(" where ").append(Bean.CUSTOMER_NAME).append(" = '").append(customerName).append('\'');
			}
			statement.execute(sql.toString());
			try (ResultSet resultSet = statement.getResultSet()) {
				LOGGER.info("Checking dynamic domain for content");

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
								Map<String, Object> json = (Map<String, Object>) JSON.unmarshall(resultSet.getString(4));
								fieldsJSON = json;
							}
							String fieldName = a.getName();
							String contentId = (String) fieldsJSON.get(fieldName);
							if (contentId != null) {
								checkContent(contentId, cm, fieldName, dynamicEntityPersistentIdentifier, t, customer, true);
							}
						}
					}
				}
			}
		}
	}

	/**
	 * Validate a single content reference.
	 *
	 * @param contentId content identifier
	 * @param cm content manager
	 * @param fieldName database field name
	 * @param persistentIdentifier table identifier
	 * @param attributeType attribute type for the field
	 * @param customer customer to validate against
	 * @param dynamicDocument true if the reference comes from a dynamic entity
	 */
	@SuppressWarnings({"java:S3776", "java:S1141"}) // complexity OK
	private void checkContent(String contentId,
								ContentManager cm,
								String fieldName,
								String persistentIdentifier,
								AttributeType attributeType,
								Customer customer,
								boolean dynamicDocument) {
		AttachmentContent content;
		try {
			content = cm.getAttachment(contentId);
			if (content == null) {
				LOGGER.error("Detected missing content {} for field name {} for table {}", contentId, fieldName, persistentIdentifier);
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
					LOGGER.error("Detected error in content {} for field name {} for table {}: Content Attribute Name {} does not match content field name {}", contentId, fieldName, persistentIdentifier, attributeName, noEmbeddedPrefixFieldName);
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
							LOGGER.error("Detected error in content {} for field name {} for table {}: Content Document {}.{} is not persistent", contentId, fieldName, persistentIdentifier, bizModule, bizDocument);
							erroneousContentCount++;
						}
						else if ((embeddedPrefixHyphenIndex < 0) && // not content through an embedded association
									(! dynamicDocument) && // not a dynamic entity
									(! persistentIdentifier.equals(p.getPersistentIdentifier()))) {
							LOGGER.error("Detected error in content {} for field name {} for table {}: Content Document {}.{} has a persistent identifier of {}", contentId, fieldName, persistentIdentifier, bizModule, bizDocument, p.getPersistentIdentifier());
							erroneousContentCount++;
						}
						else {
							Attribute a = d.getPolymorphicAttribute(customer, attributeName);

							if (a == null) {
								LOGGER.error("Detected error in content {} for field name {} for table {}: Content Attribute Name {} does not exist for document {}.{}", contentId, fieldName, persistentIdentifier, attributeName, bizModule, bizDocument);
								erroneousContentCount++;
							}
							else {
								AttributeType type = a.getAttributeType();
								if (type != attributeType) {
									LOGGER.error("Detected error in content {} for field name {} for table {}: Content Attribute Name {} is not a(n) {} for document {}.{}", contentId, fieldName, persistentIdentifier, fieldName, attributeType, bizModule, bizDocument);
									erroneousContentCount++;
								}
							}
						}
					}
					catch (@SuppressWarnings("unused") Exception e) {
						LOGGER.error("Detected error in content {} for field name {} for table {}: Content Document {}.{} does not exist for customer {}", contentId, fieldName, persistentIdentifier, bizModule, bizDocument, customer.getName());
						erroneousContentCount++;
					}
				}
			}
		}
		catch (Exception e) {
			LOGGER.error("Error checking content {} for field name {} for table {}", contentId, fieldName, persistentIdentifier);
			e.printStackTrace();
		}
	}
	
	private Collection<Table> tablesForAllCustomers = null;
	
	/**
	 * Determine if a content reference points to no backing data.
	 *
	 * @param contentId content identifier
	 * @param customer customer to validate against
	 * @return a string describing the bogus reference, or null if not found
	 * @throws Exception if validation fails
	 */
	public @Nullable String bogusContentReference(@Nonnull String contentId, @Nonnull Customer customer) throws Exception {
		if (tablesForAllCustomers == null) {
			tablesForAllCustomers = BackupUtil.getTablesForAllCustomers();
		}
		String dynamicEntityPersistentIdentifier = RDBMSDynamicPersistenceListModel.getDynamicEntityPersistent(customer).getPersistentIdentifier();

		String result = bogusStaticContentReference(contentId);
		if ((result == null) && (dynamicEntityPersistentIdentifier != null)) {
			result = bogusDynamicContentReference(contentId, dynamicEntityPersistentIdentifier);
		}
		
		return result;
	}
	
	/**
	 * Check static tables for a bogus content reference.
	 *
	 * @param contentId content identifier
	 * @return a string describing the bogus reference, or null if not found
	 */
	@SuppressWarnings("java:S3776") // complexity OK
	private @Nullable String bogusStaticContentReference(@Nonnull String contentId) {
		Persistence p = CORE.getPersistence();
		StringBuilder sql = new StringBuilder(128);
		
		for (Table table : tablesForAllCustomers) {
			sql.setLength(0);

			for (String name : table.fields.keySet()) {
				AttributeType attributeType = table.fields.get(name).getAttributeType();
				if (AttributeType.content.equals(attributeType) || AttributeType.image.equals(attributeType)) {
					if (sql.isEmpty()) {
						sql.append("select bizId from ").append(table.persistentIdentifier).append(" where ");
					}
					else {
						sql.append(" or ");
					}
					sql.append(name).append(" = :contentId");
				}
			}

			if (! sql.isEmpty()) {
				String rowBizId = p.newSQL(sql.toString()).putParameter("contentId", contentId, false).scalarResult(String.class);
				if (rowBizId != null) {
					return table.agnosticIdentifier + '#' + rowBizId;
				}
			}
		}
		
		return null;
	}
	
	/**
	 * Check dynamic entities for a bogus content reference.
	 *
	 * @param contentId content identifier
	 * @param dynamicEntityPersistentIdentifier persistent identifier for DynamicEntity
	 * @return a string describing the bogus reference, or null if not found
	 */
	private static @Nullable String bogusDynamicContentReference(@Nonnull String contentId, @Nonnull String dynamicEntityPersistentIdentifier) {
		ProvidedRepository r = ProvidedRepositoryFactory.get();
		Persistence p = CORE.getPersistence();
		
		// Find any DynamicEntity with the given contentId as a value in the fields JSON
		StringBuilder like = new StringBuilder(64).append("%\":\"").append(contentId).append("\"%");
		SQL q = p.newSQL("select bizId, bizCustomer, moduleName, documentName, fields from " + dynamicEntityPersistentIdentifier + " where fields like :like");
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
				Attribute a = (c == null) ? null : d.getPolymorphicAttribute(c, attributeName);
				if (a != null) {
					AttributeType t = a.getAttributeType();
					if ((t == AttributeType.content) || (t == AttributeType.image)) {
						return dynamicEntityPersistentIdentifier + '#' + row[0];
					}
				}
			}
		}

		return null;
	}
	
	/**
	 * Test whether a table has any content or image attributes.
	 *
	 * @param table table metadata
	 * @return true if table contains content/image fields
	 */
	private static boolean hasContent(@Nonnull Table table) {
		for (String name : table.fields.keySet()) {
			AttributeType attributeType = table.fields.get(name).getAttributeType();
			if (AttributeType.content.equals(attributeType) || AttributeType.image.equals(attributeType)) {
				return true;
			}
		}
		return false;
	}
}
