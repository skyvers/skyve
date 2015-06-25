package org.skyve.wildcat.tools.backup;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;

class Table {
	String name;
	LinkedHashMap<String, AttributeType> fields = new LinkedHashMap<>();
	List<String> relativeContentPaths = new ArrayList<>();

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

	void addFieldsFromDocument(Document document) {
		relativeContentPaths.add(document.getOwningModuleName() + File.separator + document.getName());

		fields.put(Bean.DOCUMENT_ID, AttributeType.text);
		fields.put(PersistentBean.VERSION_NAME, AttributeType.integer);
		fields.put(PersistentBean.LOCK_NAME, AttributeType.text);
		fields.put(Bean.BIZ_KEY, AttributeType.text);
		fields.put(Bean.CUSTOMER_NAME, AttributeType.text);
		fields.put(PersistentBean.FLAG_COMMENT_NAME, AttributeType.text);
		fields.put(Bean.DATA_GROUP_ID, AttributeType.text);
		fields.put(Bean.USER_ID, AttributeType.text);
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

		for (Attribute attribute : document.getAttributes()) {
			if (attribute.isPersistent()) {
				AttributeType attributeType = attribute.getAttributeType();
				if (attributeType == AttributeType.association) {
					fields.put(attribute.getName() + "_id", attribute.getAttributeType());
				}
				else if ((attributeType != AttributeType.collection) && (attributeType != AttributeType.inverse)) {
					fields.put(attribute.getName(), attributeType);
				}
			}
		}
	}
}
