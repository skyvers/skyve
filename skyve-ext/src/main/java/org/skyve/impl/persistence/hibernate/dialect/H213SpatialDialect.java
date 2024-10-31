package org.skyve.impl.persistence.hibernate.dialect;

/**
 * H2 1.3 and above dialect based on GeoDB that supports spatial.
 * This dialect generates no unique constraints as NULLs are included in unique constraint checking.
 */
public class H213SpatialDialect extends AbstractH2SpatialDialect {
	private static final long serialVersionUID = 1443609214665730024L;

	public H213SpatialDialect() {
		uniqueDelegate = new NoOpUniqueDelegate(this);
	}
}
