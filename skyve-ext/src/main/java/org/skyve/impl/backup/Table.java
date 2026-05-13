package org.skyve.impl.backup;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.JSON;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

/**
 * Represents backup table metadata derived from Skyve documents, including
 * persistent identifiers, fields, and indexes used for backup/restore.
 *
 * <p>Instances are populated from document metadata (including associations and
 * embedded structures) and can be serialized to or reconstructed from JSON for
 * portability.</p>
 */
class Table {
	/** Shared field definition for text columns with no sensitivity. */
	static final BackupField TEXT = new BackupField(AttributeType.text, Sensitivity.none);
	/** Shared field definition for association columns with no sensitivity. */
	static final BackupField ASSOCIATION = new BackupField(AttributeType.association, Sensitivity.none);
	/** Shared field definition for integer columns with no sensitivity. */
	static final BackupField INTEGER = new BackupField(AttributeType.integer, Sensitivity.none);

    private static final Logger COMMAND_LOGGER = Category.COMMAND.logger();

	/**
	 * Schema-agnostic table identifier used for equality and JSON payloads.
	 */
	String agnosticIdentifier;
	
	/**
	 * Fully qualified persistent identifier (includes catalog/schema when set).
	 */
	String persistentIdentifier;
	
	/**
	 * Ordered field definitions keyed by column name.
	 */
	LinkedHashMap<String, BackupField> fields = new LinkedHashMap<>();
	
	/**
	 * Index definitions keyed by column name.
	 */
	TreeMap<String, IndexType> indexes = new TreeMap<>();
	
	/**
	 * Create a table metadata holder for backup/restore.
	 *
	 * @param agnosticIdentifier schema-agnostic table identifier
	 * @param persistentIdentifier fully qualified persistent identifier
	 */
	Table(String agnosticIdentifier, String persistentIdentifier) {
		this.agnosticIdentifier = agnosticIdentifier;
		this.persistentIdentifier = persistentIdentifier;
	}

	/**
	 * Equality is based on the schema-agnostic identifier.
	 */
	@Override
	public boolean equals(Object obj) {
		return ((obj instanceof Table table) && (agnosticIdentifier != null) && agnosticIdentifier.equals(table.agnosticIdentifier));
	}

	/**
	 * Hash code is derived from the schema-agnostic identifier.
	 */
	@Override
	public int hashCode() {
		return agnosticIdentifier.hashCode();
	}

	/**
	 * Populate backup fields and indexes from a document's metadata.
	 *
	 * <p>This captures Skyve system columns, discriminator/type markers for
	 * inheritance and arc associations, and embedded attributes, while skipping
	 * dynamic attributes and non-persistent collections.</p>
	 *
	 * @param customer the owning customer used to resolve modules/documents
	 * @param document the document whose metadata drives the backup schema
	 */
	void addFieldsFromDocument(Customer customer, Document document) {
		boolean joinedExtension = false;
		Persistent persistent = document.getPersistent();
		if (persistent != null) {
			ExtensionStrategy strategy = persistent.getStrategy();
			// Check there is actually a Document with a persistent name extended somewhere in the document hierarchy
			Module module = customer.getModule(document.getOwningModuleName());
			if (ProvidedRepositoryFactory.get().findNearestPersistentSingleOrJoinedSuperDocument(customer, module, document) != null) {
				if (ExtensionStrategy.single.equals(strategy)) {
					fields.put(PersistentBean.DISCRIMINATOR_NAME, TEXT);
				}
				else if (ExtensionStrategy.joined.equals(strategy)) {
					if (document.getExtends() != null) {
						joinedExtension = true;
					}
				}
			}
		}
		
		fields.put(Bean.DOCUMENT_ID, TEXT);
		if (! joinedExtension) { 
			fields.put(PersistentBean.VERSION_NAME, INTEGER);
			fields.put(PersistentBean.LOCK_NAME, TEXT);
			Sensitivity sensitivity = document.getBizKeySensitity();
			if ((sensitivity == null) || (sensitivity == Sensitivity.none)) {
				fields.put(Bean.BIZ_KEY, TEXT);
			}
			else {
				fields.put(Bean.BIZ_KEY, new BackupField(AttributeType.text, sensitivity));
			}
			fields.put(Bean.CUSTOMER_NAME, TEXT);
			fields.put(PersistentBean.FLAG_COMMENT_NAME, TEXT);
			fields.put(Bean.DATA_GROUP_ID, TEXT);
			fields.put(Bean.USER_ID, TEXT);
		}
		String parentDocumentName = document.getParentDocumentName();
		if (parentDocumentName != null) {
			if (parentDocumentName.equals(document.getName())) { // hierarchical
				fields.put(HierarchicalBean.PARENT_ID, TEXT);
			}
			else {
				fields.put(ChildBean.PARENT_NAME + "_id", ASSOCIATION);
			}

			if (document.isOrdered()) {
				fields.put(Bean.ORDINAL_NAME, INTEGER);
			}
		}
		
		processAttributes(joinedExtension, null, customer, document);
	}
	
	/**
	 * Walk attributes (and embedded associations) and register their backup
	 * fields and indexes.
	 *
	 * @param joinedExtension whether this document is a joined extension
	 * @param embeddedColumnsPrefix prefix for embedded attribute columns, or null
	 * @param customer the owning customer used to resolve referenced documents
	 * @param document the document whose attributes are being processed
	 */
	private void processAttributes(boolean joinedExtension, String embeddedColumnsPrefix, Customer customer, Document document) {
		for (Attribute attribute : joinedExtension ? document.getAttributes() : document.getAllAttributes(customer)) {
			if (attribute.isPersistent()) {
				String attributeName = attribute.getName();
				String fieldName = (embeddedColumnsPrefix == null) ? attributeName : (embeddedColumnsPrefix + '_' + attributeName);
				AttributeType attributeType = attribute.getAttributeType();
				if (attributeType == AttributeType.association) {
					// Check if this is an arc and add the "type" field if so
					Association association = (Association) attribute;
					Document referencedDocument = customer.getModule(document.getOwningModuleName()).getDocument(customer, association.getDocumentName());
					if (! referencedDocument.isDynamic()) {
						if (association.getType() == AssociationType.embedded) {
							processAttributes(false, association.getEmbeddedColumnsPrefix(), customer, referencedDocument);
						}
						else {
							Persistent referencedPersistent = referencedDocument.getPersistent();
							if ((referencedPersistent != null) && referencedPersistent.isPolymorphicallyMapped()) {
								fields.put(attributeName + "_type", TEXT);
							}
							
							fields.put(attributeName + "_id", new BackupField(attributeType, Sensitivity.none));
							if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("{} - Put {}_id -> {}", agnosticIdentifier, attributeName, attributeType);
						}
					}
				}
				else if ((! AttributeType.collection.equals(attributeType)) && 
							(! AttributeType.inverseOne.equals(attributeType)) &&
							(! AttributeType.inverseMany.equals(attributeType))) {
					boolean dynamic = false;
					if (attribute instanceof Field field) {
						dynamic = field.isDynamic();
						IndexType indexType = field.getIndex();
						if (indexType != null) {
							indexes.put(fieldName, indexType);
						}
					}
					if (! dynamic) {
						// Ensure sensitivity is always a non null value
						Sensitivity sensitivity = attribute.getSensitivity();
						if (sensitivity == null) {
							sensitivity = Sensitivity.none;
						}

						// Either add the field, or upgrade its sensitivity value as appropriate
						BackupField field = fields.get(fieldName);
						if (field == null) {
							// If a length field, record the maximum length
							if (attribute instanceof LengthField lengthField) {
								Integer maxLength = Integer.valueOf(lengthField.getLength());
								fields.put(fieldName, new BackupLengthField(attributeType, sensitivity, maxLength));
							} else {
								fields.put(fieldName, new BackupField(attributeType, sensitivity));
							}

							if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("{} - Put {} -> {}", agnosticIdentifier, fieldName, attributeType);
						}
						else {
							Sensitivity existing = field.getSensitivity();
							if (sensitivity.ordinal() > existing.ordinal()) {
								field.setSensitivity(sensitivity);
							}
						}
					}
				}
			}
		}
	}
	
	/**
	 * Serialize this table definition to JSON for backup/restore portability.
	 *
	 * <p>The JSON includes the table name, fields (attribute type only), and for
	 * join tables the owner table name and ordering flag.</p>
	 *
	 * @return JSON representation of this table definition
	 * @throws Exception if JSON marshalling fails
	 */
	public String toJSON() throws Exception {
		Map<String, Object> result = new TreeMap<>();
		result.put("name", agnosticIdentifier);
		List<Map<String, Object>> fieldList = new ArrayList<>(fields.size());
		for (String key : fields.keySet()) {
			BackupField backupField = fields.get(key);
			Map<String, Object> field = new TreeMap<>();
			// Ignore data sensitivity in fields as this is just for restore
			field.put(key, backupField.getAttributeType());
			fieldList.add(field);
		}
		result.put("fields", fieldList);
		if (this instanceof JoinTable join) {
			result.put("ownerTableName", join.ownerAgnosticIdentifier);
			result.put("ordered", Boolean.valueOf(join.ordered));
		}
		return JSON.marshall(result);
	}
	
	/**
	 * Reconstruct a table definition from its JSON representation.
	 *
	 * <p>Schema/catalog qualifiers are applied when configured, and join-table
	 * metadata is restored when present.</p>
	 *
	 * @param json the JSON representation previously produced by {@link #toJSON()}
	 * @return the reconstructed table definition
	 * @throws Exception if JSON parsing or conversion fails
	 */
	public static Table fromJSON(String json) throws Exception {
		Map<String, Object> map = (Map<String, Object>) JSON.unmarshall(json);

		String agnosticIdentifier = (String) map.get("name");
		String persistentIdentifier = agnosticIdentifier;
		String ownerAgnosticIdentifier = (String) map.get("ownerTableName");
		String ownerPersistentIdentifier = ownerAgnosticIdentifier;

		if ((UtilImpl.CATALOG != null) || (UtilImpl.SCHEMA != null)) {
			if (agnosticIdentifier.indexOf('.') < 0) { // no schema or catalog
				persistentIdentifier = Persistent.determinePersistentIdentifier(agnosticIdentifier, UtilImpl.CATALOG, UtilImpl.SCHEMA);
			}
			if ((ownerAgnosticIdentifier != null) && (ownerAgnosticIdentifier.indexOf('.') < 0)) { // no schema or catalog
				ownerPersistentIdentifier = Persistent.determinePersistentIdentifier(ownerAgnosticIdentifier, UtilImpl.CATALOG, UtilImpl.SCHEMA);
			}
		}
		
		Boolean ordered = (Boolean) map.get("ordered");
		Table result = (ownerAgnosticIdentifier == null) ? 
						new Table(agnosticIdentifier, persistentIdentifier) :
						new JoinTable(agnosticIdentifier, persistentIdentifier, ownerAgnosticIdentifier, ownerPersistentIdentifier, Boolean.TRUE.equals(ordered));

		List<Map<String, Object>> fieldList = (List<Map<String, Object>>) map.get("fields");
		for (Map<String, Object> field : fieldList) {
			for (String key : field.keySet()) {
				// Ignore data sensitivity in fields as this is just for restore
				result.fields.put(key, new BackupField(AttributeType.valueOf((String) field.get(key)), Sensitivity.none));
			}
		}
		
		return result;
	}
}
