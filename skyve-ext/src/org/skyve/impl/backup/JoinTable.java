package org.skyve.impl.backup;

import org.skyve.domain.PersistentBean;
import org.skyve.metadata.model.Attribute.AttributeType;

class JoinTable extends Table {
	String ownerTableName;

	JoinTable(String name, String ownerTableName) {
		super(name);
		this.ownerTableName = ownerTableName;
		
		fields.put(PersistentBean.OWNER_COLUMN_NAME, AttributeType.text);
		fields.put(PersistentBean.ELEMENT_COLUMN_NAME, AttributeType.text);
	}
}
