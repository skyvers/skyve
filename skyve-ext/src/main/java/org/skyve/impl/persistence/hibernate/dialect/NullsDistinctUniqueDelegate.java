package org.skyve.impl.persistence.hibernate.dialect;

import org.hibernate.dialect.Dialect;
import org.hibernate.dialect.unique.DefaultUniqueDelegate;
import org.hibernate.mapping.UniqueKey;

/**
 * A unique delegate that generates an ANSI SQL 2016 UNIQUE NULLS DISTINCT clause.
 */
public class NullsDistinctUniqueDelegate extends DefaultUniqueDelegate {
	public NullsDistinctUniqueDelegate(Dialect dialect) {
		super(dialect);
	}
	
	@Override
	protected String uniqueConstraintSql(UniqueKey uniqueKey) {
		String result = super.uniqueConstraintSql(uniqueKey);
		return result.replace("unique (", "unique nulls distinct (");
	}
}
