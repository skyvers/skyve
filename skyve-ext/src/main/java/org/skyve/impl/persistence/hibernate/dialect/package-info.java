/**
 * Skyve-specific Hibernate dialect wrappers for spatial and uniqueness support.
 *
 * <p>Each dialect class combines a database-vendor spatial dialect from
 * Hibernate Spatial with the {@link org.skyve.impl.persistence.hibernate.dialect.SkyveDialect}
 * interface. Supported databases and versions include PostgreSQL 8.2–10,
 * MySQL 5, 5.6, and 8, SQL Server 2008 and 2012+.
 *
 * <p>Uniqueness delegates ({@code NullsDistinctUniqueDelegate},
 * {@code SQLServer2008NullTolerantUniqueDelegate}, {@code NoOpUniqueDelegate})
 * customise null-distinct unique-constraint handling per vendor.
 *
 * <p>{@code DDLDelegate} drives Hibernate schema migration scripts.
 *
 * @see org.skyve.impl.persistence.hibernate.dialect.mysqlbugfix
 */
package org.skyve.impl.persistence.hibernate.dialect;
