/**
 * Hibernate-based persistence context implementations for Skyve.
 *
 * <p>{@code AbstractHibernatePersistence} provides the core Hibernate session
 * lifecycle. Concrete variants are {@code HibernateContentPersistence} (with
 * content-store integration) and {@code HibernateNoContentPersistence} (without).
 * {@code SQLServer2016HibernateContentPersistence} specialises behaviour for
 * SQL Server 2016+. Supporting classes include {@code HibernateBizQL},
 * {@code HibernateDocumentQuery}, {@code HibernateSQL}, {@code HibernateListener},
 * and {@code HibernateAutoClosingIterable}.
 */
package org.skyve.impl.persistence.hibernate;
