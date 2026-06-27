/**
 * Skyve's persistence abstraction layer for querying and managing domain beans.
 *
 * <h2>Core types</h2>
 * <ul>
 *   <li>{@link org.skyve.persistence.Persistence} — the central service for transactions,
 *       CRUD operations, and query construction. Obtained via
 *       {@link org.skyve.CORE#getPersistence()}.
 *   <li>{@link org.skyve.persistence.DocumentQuery} — type-safe, metadata-driven query
 *       builder for retrieving domain beans with projections, joins, filters, ordering,
 *       and aggregation.
 *   <li>{@link org.skyve.persistence.DocumentFilter} — predicate builder for
 *       {@link org.skyve.persistence.DocumentQuery}; supports comparison, range, NULL,
 *       set membership, LIKE, spatial operators, and NULL-tolerant variants.
 *   <li>{@link org.skyve.persistence.BizQL} — Skyve's HQL-like object query language
 *       that operates directly over domain bean types.
 *   <li>{@link org.skyve.persistence.SQL} — native SQL execution with type-safe
 *       Skyve-type parameter binding.
 * </ul>
 *
 * <h2>Query result interfaces</h2>
 * <p>Queries implement one or more result-fetching interfaces depending on the supported
 * return shape:
 * <ul>
 *   <li>{@link org.skyve.persistence.BeanQuery} — returns typed {@link org.skyve.domain.Bean} instances.
 *   <li>{@link org.skyve.persistence.ProjectedQuery} — returns beans populated from
 *       projected column aliases.
 *   <li>{@link org.skyve.persistence.ScalarQuery} — returns single-column scalar values.
 *   <li>{@link org.skyve.persistence.TupleQuery} — returns {@code Object[]} rows.
 *   <li>{@link org.skyve.persistence.DMLQuery} — executes data-modification statements.
 * </ul>
 *
 * <h2>Streaming</h2>
 * <p>{@link org.skyve.persistence.AutoClosingIterable} wraps a database cursor for lazy,
 * streaming iteration. Always use it in a try-with-resources block to guarantee the
 * cursor is closed:
 * <pre>{@code
 * try (AutoClosingIterable<MyBean> rows = query.beanIterable()) {
 *     for (MyBean row : rows) { ... }
 * }
 * }</pre>
 *
 * <h2>Threading</h2>
 * <p>{@link org.skyve.persistence.Persistence} instances are <em>thread-confined</em>;
 * they must not be shared across threads. Each request or job thread obtains its own
 * instance via {@link org.skyve.CORE#getPersistence()}.
 *
 * <h2>Dynamic persistence</h2>
 * <p>{@link org.skyve.persistence.DynamicPersistence} extends the contract for data stores
 * (e.g. graph databases) that support dynamic domain schemas. It is used internally by
 * the framework and rarely called from application code.
 */
package org.skyve.persistence;
