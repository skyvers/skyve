package org.skyve.impl.backup;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;

import jakarta.annotation.Nonnull;

class JoinTable extends Table {
	String ownerAgnosticIdentifier;
	String ownerPersistentIdentifier;
	boolean ordered;
	
	JoinTable(@Nonnull String agnosticIdentifier,
				@Nonnull String persistentIdentifier,
				@Nonnull String ownerAgnosticIdentifier,
				@Nonnull String ownerPersistentIdentifier,
				boolean ordered) {
		super(agnosticIdentifier, persistentIdentifier);
		this.ownerAgnosticIdentifier = ownerAgnosticIdentifier;
		this.ownerPersistentIdentifier = ownerPersistentIdentifier;
		this.ordered = ordered;
		
		fields.put(PersistentBean.OWNER_COLUMN_NAME, Table.TEXT);
		fields.put(PersistentBean.ELEMENT_COLUMN_NAME, Table.TEXT);
		if (ordered) {
			fields.put(Bean.ORDINAL_NAME, Table.INTEGER);
		}
	}
}
