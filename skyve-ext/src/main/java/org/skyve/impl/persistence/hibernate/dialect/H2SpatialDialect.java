package org.skyve.impl.persistence.hibernate.dialect;

/**
 * This should extend the latest H2 dialect in lock step with the Skyve H2 dependency.
 * This exists to keep the JSONs compatible.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class H2SpatialDialect extends H222SpatialDialect {
	private static final long serialVersionUID = 1943367186123412392L;
}
