/**
 * Skyve custom Hibernate SQL dialect definitions.
 *
 * <p>{@code SkyveDialect} extends the Hibernate dialect mechanism to register
 * Skyve-specific SQL functions, type mappings, and optimisations that are common
 * across all supported database platforms. Concrete platform dialects in
 * {@code skyve-ext} subclass {@code SkyveDialect} to add database-specific
 * SQL generation rules.
 *
 * @see org.skyve.impl.persistence
 */
package org.skyve.impl.persistence.hibernate.dialect;
