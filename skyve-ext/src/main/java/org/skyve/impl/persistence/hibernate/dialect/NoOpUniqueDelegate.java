package org.skyve.impl.persistence.hibernate.dialect;

import org.hibernate.boot.Metadata;
import org.hibernate.dialect.Dialect;
import org.hibernate.dialect.unique.DefaultUniqueDelegate;
import org.hibernate.mapping.UniqueKey;

/**
 * Creates no unique constraints.
 * 
 * @author mike
 */
public class NoOpUniqueDelegate extends DefaultUniqueDelegate {
	public NoOpUniqueDelegate(Dialect dialect) {
		super(dialect);
	}
	
	@Override
	public String getAlterTableToAddUniqueKeyCommand(UniqueKey uniqueKey, Metadata metadata) {
		return "";
	}
	
	@Override
	public String getAlterTableToDropUniqueKeyCommand(UniqueKey uniqueKey, Metadata metadata) {
		return "";
	}
}
