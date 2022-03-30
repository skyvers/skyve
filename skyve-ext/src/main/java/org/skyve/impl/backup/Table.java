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
		}
		String parentDocumentName = document.getParentDocumentName();
		if (parentDocumentName != null) {
			if (parentDocumentName.equals(document.getName())) { // hierarchical
				fields.put(HierarchicalBean.PARENT_ID, AttributeType.text);
			}
			else {
				fields.put(ChildBean.PARENT_NAME + "_id", AttributeType.association);
			}

			if (document.isOrdered()) {
				fields.put(Bean.ORDINAL_NAME, AttributeType.integer);
			}
		}
		
		for (Attribute attribute : joinedExtension ? document.getAttributes() : document.getAllAttributes(customer)) {
			if (attribute.isPersistent()) {
				String attributeName = attribute.getName();
				AttributeType attributeType = attribute.getAttributeType();
				if (attributeType == AttributeType.association) {
					// Check if this is an arc and add the "type" field if so
					Association association = (Association) attribute;
					Document referencedDocument = customer.getModule(document.getOwningModuleName()).getDocument(customer, association.getDocumentName());
					if (! referencedDocument.isDynamic()) {
						Persistent referencedPersistent = referencedDocument.getPersistent();
						if ((referencedPersistent != null) && ExtensionStrategy.mapped.equals(referencedPersistent.getStrategy())) {
							fields.put(attributeName + "_type", AttributeType.text);
						}
						
						fields.put(attributeName + "_id", attribute.getAttributeType());
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
							indexes.put(attributeName, indexType);
						}
					}
					if (! dynamic) {
						fields.put(attributeName, attributeType);
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
			Map<String, Object> field = new TreeMap<>();
			field.put(key, fields.get(key));
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
				result.fields.put(key, AttributeType.valueOf((String) field.get(key)));
			}
		}
		
		return result;
	}
}
