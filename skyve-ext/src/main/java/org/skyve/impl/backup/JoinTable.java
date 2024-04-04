package org.skyve.impl.backup;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;

class JoinTable extends Table {
	String ownerTableName;
	boolean ordered;
	
	JoinTable(String name, String ownerTableName, boolean ordered) {
		super(name);
		this.ownerTableName = ownerTableName;
		this.ordered = ordered;
		
		fields.put(PersistentBean.OWNER_COLUMN_NAME, Table.TEXT);
		fields.put(PersistentBean.ELEMENT_COLUMN_NAME, Table.TEXT);
		if (ordered) {
			fields.put(Bean.ORDINAL_NAME, Table.INTEGER);
		}
	}
}
