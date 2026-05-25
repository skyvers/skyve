/**
 * Abstract persistence infrastructure and query base classes for the Skyve persistence layer.
 *
 * <p>This package provides the framework contracts implemented by persistence back-ends
 * (typically Hibernate/JPA). None of the classes here depend on a specific ORM; they
 * express the persistence contract in terms of Skyve abstractions.
 *
 * <p>Key classes:
 * <ul>
 *   <li>{@code AbstractPersistence} — the primary thread-confined persistence context;
 *       owns the transaction lifecycle, CRUD operations, and query execution.
 *       <em>Threading: thread-confined</em> — one instance per request or background
 *       thread; must not be shared across threads.
 *   <li>{@code AbstractDocumentQuery} / {@code AbstractBizQL} / {@code AbstractSQL} /
 *       {@code AbstractQuery} — base classes for the three query styles (document-object
 *       queries, Skyve BizQL, and raw SQL); carry parameter binding, paging, and
 *       timeout state.
 *   <li>{@code DocumentFilterImpl} — mutable filter expression builder used by
 *       {@code DocumentQuery}.
 *   <li>{@code DynaIterable} — a lazy, streaming iterable over large query result sets.
 *   <li>{@code NamedParameterPreparedStatement} — a JDBC helper that supports named
 *       {@code :param} placeholders in raw SQL statements.
 * </ul>
 *
 * <p>The {@code hibernate} sub-package contains the Hibernate-specific dialect extension.
 *
 * @see org.skyve.persistence.DocumentQuery
 * @see org.skyve.persistence.BizQL
 */
package org.skyve.impl.persistence;
