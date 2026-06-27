package org.skyve.impl.persistence.hibernate.dialect;

/**
 * H2 2.2 and above dialect based on GeoDB that supports spatial.
 * This dialect generates unique constraints with an ANSI SQL 2016 NULLS DISTINCT clause.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class H222SpatialDialect extends H213SpatialDialect {
	private static final long serialVersionUID = 5392219267730830548L;

	/**
	 * Creates the H2 2.2 spatial dialect with a {@code NULLS DISTINCT} unique-key delegate.
	 */
	public H222SpatialDialect() {
		uniqueDelegate = new NullsDistinctUniqueDelegate(this);
	}
}
