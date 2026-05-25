/**
 * Base metadata contracts and enumerations shared across all Skyve metadata types.
 *
 * <h2>Metadata hierarchy</h2>
 * <p>All Skyve metadata objects implement {@link org.skyve.metadata.MetaData} as a
 * common marker. Sub-interfaces refine the contract:
 * <ul>
 *   <li>{@link org.skyve.metadata.SerializableMetaData} — metadata that can be
 *       serialized (all runtime metadata).
 *   <li>{@link org.skyve.metadata.NamedMetaData} — metadata that has a name
 *       (modules, documents, attributes, queries, etc.).
 *   <li>{@link org.skyve.metadata.DecoratedMetaData} — metadata that supports
 *       arbitrary key-value property extensions.
 *   <li>{@link org.skyve.metadata.PersistentMetaData} — metadata that records
 *       its last-modified timestamp for cache invalidation.
 *   <li>{@link org.skyve.metadata.ReloadableMetaData} — metadata that can be hot-reloaded
 *       and tracks when it was last checked.
 * </ul>
 *
 * <h2>Query and sort types</h2>
 * <ul>
 *   <li>{@link org.skyve.metadata.FilterOperator} — the set of comparison operators
 *       available in query filters and list view column filters.
 *   <li>{@link org.skyve.metadata.SortDirection} — ascending or descending sort.
 *   <li>{@link org.skyve.metadata.Ordering} — a binding + sort-direction pair used
 *       to express multi-column orderings.
 * </ul>
 *
 * <h2>Named converters and formatters</h2>
 * <ul>
 *   <li>{@link org.skyve.metadata.ConverterName} — an enumeration of all built-in
 *       {@link org.skyve.domain.types.converters.Converter} implementations. Used in
 *       attribute metadata to specify how values are converted to and from display strings.
 *   <li>{@link org.skyve.metadata.FormatterName} — an enumeration of all built-in
 *       formatters for read-only display of values.
 * </ul>
 */
package org.skyve.metadata;
