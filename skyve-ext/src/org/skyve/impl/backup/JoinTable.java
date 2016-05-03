package org.skyve.impl.backup;

import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.impl.backup.Table;

class JoinTable extends Table {
	String ownerTableName;

	JoinTable(String name, String ownerTableName) {
		super(name);
		this.ownerTableName = ownerTableName;

		fields.put("owner_id", AttributeType.text);
		fields.put("element_id", AttributeType.text);
	}
}
