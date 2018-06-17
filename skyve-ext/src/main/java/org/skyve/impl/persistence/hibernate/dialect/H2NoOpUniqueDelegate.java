package org.skyve.impl.persistence.hibernate.dialect;

import org.hibernate.boot.Metadata;
import org.hibernate.dialect.Dialect;
import org.hibernate.dialect.unique.DefaultUniqueDelegate;
import org.hibernate.mapping.UniqueKey;

/**
 * Create no H2 unique constraints even when asked to since H2 considers NULL to be a value and raises a constraint violation.
 * 
 * @author mike
 */
public class H2NoOpUniqueDelegate extends DefaultUniqueDelegate {
	public H2NoOpUniqueDelegate(Dialect dialect) {
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
