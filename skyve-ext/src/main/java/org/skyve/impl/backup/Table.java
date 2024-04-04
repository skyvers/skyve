package org.skyve.impl.backup;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.MutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
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

class Table {
	static final ImmutablePair<AttributeType, Sensitivity> TEXT = ImmutablePair.of(AttributeType.text, Sensitivity.none);
	static final ImmutablePair<AttributeType, Sensitivity> ASSOCIATION = ImmutablePair.of(AttributeType.association, Sensitivity.none);
	static final ImmutablePair<AttributeType, Sensitivity> INTEGER = ImmutablePair.of(AttributeType.integer, Sensitivity.none);

	String name;
	LinkedHashMap<String, Pair<AttributeType, Sensitivity>> fields = new LinkedHashMap<>();
	TreeMap<String, IndexType> indexes = new TreeMap<>();
	
	Table(String name) {
		this.name = name;
	}

	@Override
	public boolean equals(Object obj) {
		return ((obj instanceof Table) && (name != null) && name.equals(((Table) obj).name));
	}

	@Override
	public int hashCode() {
		return name.hashCode();
	}

	void addFieldsFromDocument(Customer customer, Document document) {
		boolean joinedExtension = false;
		Persistent persistent = document.getPersistent();
		if (persistent != null) {
			ExtensionStrategy strategy = persistent.getStrategy();
			// Check there is actually a Document with a persistent name extended somewhere in the document hierarchy
			Module module = customer.getModule(document.getOwningModuleName());
			if (ProvidedRepositoryFactory.get().findNearestPersistentUnmappedSuperDocument(customer, module, document) != null) {
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
			fields.put(Bean.BIZ_KEY, TEXT);
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
							if ((referencedPersistent != null) && ExtensionStrategy.mapped.equals(referencedPersistent.getStrategy())) {
								fields.put(attributeName + "_type", TEXT);
							}
							
							fields.put(attributeName + "_id", ImmutablePair.of(attribute.getAttributeType(), Sensitivity.none));
						}
					}
				}
				else if ((! AttributeType.collection.equals(attributeType)) && 
							(! AttributeType.inverseOne.equals(attributeType)) &&
							(! AttributeType.inverseMany.equals(attributeType))) {
					boolean dynamic = false;
					if (attribute instanceof Field) {
						Field field = (Field) attribute;
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
						Pair<AttributeType, Sensitivity> pair = fields.get(fieldName);
						if (pair == null) {
							fields.put(fieldName, MutablePair.of(attributeType, sensitivity));
						}
						else {
							Sensitivity existing = pair.getRight();
							if (sensitivity.ordinal() > existing.ordinal()) {
								pair.setValue(sensitivity);
							}
						}
					}
				}
			}
		}
	}
	
	public String toJSON() throws Exception {
		Map<String, Object> result = new TreeMap<>();
		result.put("name", name);
		List<Map<String, Object>> fieldList = new ArrayList<>(fields.size());
		for (String key : fields.keySet()) {
			Pair<AttributeType, Sensitivity> pair = fields.get(key);
			Map<String, Object> field = new TreeMap<>();
			// Ignore data sensitivity in fields as this is just for restore
			field.put(key, pair.getLeft());
			fieldList.add(field);
		}
		result.put("fields", fieldList);
		if (this instanceof JoinTable) {
			JoinTable join = (JoinTable) this;
			result.put("ownerTableName", join.ownerTableName);
			result.put("ordered", Boolean.valueOf(join.ordered));
		}
		return JSON.marshall(result);
	}
	
	@SuppressWarnings("unchecked")
	public static Table fromJSON(String json) throws Exception {
		Map<String, Object> map = (Map<String, Object>) JSON.unmarshall(null, json);
		String tableName = (String) map.get("name");
		String ownerTableName = (String) map.get("ownerTableName");
		Boolean ordered = (Boolean) map.get("ordered");
		Table result = (ownerTableName == null) ? 
							new Table(tableName) :
							new JoinTable(tableName, ownerTableName, Boolean.TRUE.equals(ordered));
		List<Map<String, Object>> fieldList = (List<Map<String, Object>>) map.get("fields");
		for (Map<String, Object> field : fieldList) {
			for (String key : field.keySet()) {
				// Ignore data sensitivity in fields as this is just for restore
				result.fields.put(key, ImmutablePair.of(AttributeType.valueOf((String) field.get(key)), Sensitivity.none));
			}
		}
		
		return result;
	}
}
