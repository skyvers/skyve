package org.skyve.impl.persistence.hibernate.dialect;

import org.hibernate.dialect.Dialect;
import org.hibernate.dialect.unique.DefaultUniqueDelegate;
import org.hibernate.mapping.UniqueKey;

/**
 * A unique delegate that generates an ANSI SQL 2016 UNIQUE NULLS DISTINCT clause.
 */
public class NullsDistinctUniqueDelegate extends DefaultUniqueDelegate {
	/**
	 * Creates the delegate for the supplied Hibernate dialect.
	 *
	 * @param dialect the owning Hibernate dialect
	 */
	public NullsDistinctUniqueDelegate(Dialect dialect) {
		super(dialect);
	}
	
	/**
	 * Rewrites Hibernate's unique constraint SQL to preserve null-distinct semantics.
	 *
	 * @param uniqueKey the unique key definition being rendered
	 * @return the rewritten SQL fragment containing {@code unique nulls distinct}
	 */
	@Override
	protected String uniqueConstraintSql(UniqueKey uniqueKey) {
		String result = super.uniqueConstraintSql(uniqueKey);
		return result.replace("unique (", "unique nulls distinct (");
	}
}
