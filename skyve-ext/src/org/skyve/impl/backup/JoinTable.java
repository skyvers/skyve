package org.skyve.impl.backup;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.model.Attribute.AttributeType;

class JoinTable extends Table {
	String ownerTableName;
	boolean ordered;
	
	JoinTable(String name, String ownerTableName, boolean ordered) {
		super(name);
		this.ownerTableName = ownerTableName;
		this.ordered = ordered;
		
		fields.put(PersistentBean.OWNER_COLUMN_NAME, AttributeType.text);
		fields.put(PersistentBean.ELEMENT_COLUMN_NAME, AttributeType.text);
		if (ordered) {
			fields.put(Bean.ORDINAL_NAME, AttributeType.integer);
		}
	}
}
