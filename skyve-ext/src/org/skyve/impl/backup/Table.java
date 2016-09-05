package org.skyve.impl.backup;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Document;
import org.skyve.util.JSON;

class Table {
	String name;
	LinkedHashMap<String, AttributeType> fields = new LinkedHashMap<>();
	List<String> relativeContentPaths = new ArrayList<>();
	boolean joinedExtensionOnly = true;
	
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

	void addFieldsFromDocument(Document document) throws MetaDataException {
		relativeContentPaths.add(document.getOwningModuleName() + File.separator + document.getName());

		boolean joinedExtension = false;
		Persistent persistent = document.getPersistent();
		if (persistent != null) {
			ExtensionStrategy strategy = persistent.getStrategy();
			if (ExtensionStrategy.single.equals(strategy)) {
				fields.put(PersistentBean.DISCRIMINATOR_NAME, AttributeType.text);
			}
			else if (ExtensionStrategy.joined.equals(strategy)) {
				if (document.getExtends() != null) {
					joinedExtension = true;
				}
			}
		}
		
		fields.put(Bean.DOCUMENT_ID, AttributeType.text);
		if (! joinedExtension) { 
			fields.put(PersistentBean.VERSION_NAME, AttributeType.integer);
			fields.put(PersistentBean.LOCK_NAME, AttributeType.text);
			fields.put(Bean.BIZ_KEY, AttributeType.text);
			fields.put(Bean.CUSTOMER_NAME, AttributeType.text);
			fields.put(PersistentBean.FLAG_COMMENT_NAME, AttributeType.text);
			fields.put(Bean.DATA_GROUP_ID, AttributeType.text);
			fields.put(Bean.USER_ID, AttributeType.text);
			joinedExtensionOnly = false;
		}
		String parentDocumentName = document.getParentDocumentName();
		if (parentDocumentName != null) {
			if (parentDocumentName.equals(document.getName())) { // hierarchical
				fields.put(HierarchicalBean.PARENT_ID, AttributeType.text);
			}
			else {
				fields.put(ChildBean.PARENT_NAME + "_id", AttributeType.association);
			}
		}
		if (document.isOrdered()) {
			fields.put(ChildBean.ORDINAL_KEY, AttributeType.integer);
		}
		
		for (Attribute attribute : joinedExtension ? document.getAttributes() : document.getAllAttributes()) {
			if (attribute.isPersistent()) {
				AttributeType attributeType = attribute.getAttributeType();
				if (attributeType == AttributeType.association) {
					// Check if this is an arc and add the "type" field if so
					Association association = (Association) attribute;
					Customer c = AbstractPersistence.get().getUser().getCustomer();
					Document referencedDocument = c.getModule(document.getOwningModuleName()).getDocument(c, association.getDocumentName());
					Persistent referencedPersistent = referencedDocument.getPersistent();
					if ((referencedPersistent != null) && ExtensionStrategy.mapped.equals(referencedPersistent.getStrategy())) {
						fields.put(attribute.getName() + "_type", AttributeType.text);
					}
					
					fields.put(attribute.getName() + "_id", attribute.getAttributeType());
				}
				else if ((! AttributeType.collection.equals(attributeType)) && 
							(! AttributeType.inverseOne.equals(attributeType)) &&
							(! AttributeType.inverseMany.equals(attributeType))) {
					fields.put(attribute.getName(), attributeType);
				}
			}
		}
	}
	
	public String toJSON() throws Exception {
		Map<String, Object> result = new TreeMap<>();
		result.put("name", name);
		List<Map<String, Object>> fieldList = new ArrayList<>(fields.size());
		for (String key : fields.keySet()) {
			Map<String, Object> field = new TreeMap<>();
			field.put(key, fields.get(key));
			fieldList.add(field);
		}
		result.put("fields", fieldList);
		result.put("relativeContentPaths", relativeContentPaths);
		if (joinedExtensionOnly) {
			result.put("joinedExtensionOnly", Boolean.valueOf(joinedExtensionOnly));
		}
		if (this instanceof JoinTable) {
			result.put("ownerTableName", ((JoinTable) this).ownerTableName);
		}
		return JSON.marshall(null, result, null);
	}
	
	@SuppressWarnings("unchecked")
	public static Table fromJSON(String json) throws Exception {
		Map<String, Object> map = (Map<String, Object>) JSON.unmarshall(null, json);
		String tableName = (String) map.get("name");
		String ownerTableName = (String) map.get("ownerTableName");
		boolean joinedExtensionOnly = Boolean.TRUE.equals(map.get("joinedExtensionOnly"));
		Table result = (ownerTableName == null) ? 
							new Table(tableName) :
							new JoinTable(tableName, ownerTableName);
		result.joinedExtensionOnly = joinedExtensionOnly;
		List<Map<String, Object>> fieldList = (List<Map<String, Object>>) map.get("fields");
		for (Map<String, Object> field : fieldList) {
			for (String key : field.keySet()) {
				result.fields.put(key, AttributeType.valueOf((String) field.get(key)));
			}
		}
		result.relativeContentPaths = (List<String>) map.get("relativeContentPaths");
		
		return result;
	}
}
