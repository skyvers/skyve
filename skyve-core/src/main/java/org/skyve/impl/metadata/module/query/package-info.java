/**
 * Internal implementations of the {@link org.skyve.metadata.module.query} query
 * definition and column contracts.
 *
 * <p>Query definitions describe how data is fetched for list views. The hierarchy:
 * <ul>
 *   <li>{@code QueryDefinitionImpl} — base implementation; carries module reference,
 *       document target, and the default columns list.
 *   <li>{@code MetaDataQueryDefinitionImpl} — extends with JAXB-annotated metadata
 *       query fields; implements
 *       {@link org.skyve.metadata.module.query.MetaDataQueryDefinition}.
 *   <li>{@code BizQLDefinitionImpl} — BizQL (Hibernate-dialect) query implementation.
 *   <li>{@code SQLDefinitionImpl} — raw SQL query implementation.
 *   <li>{@code MetaDataQueryReferenceImpl}, {@code BizQLReferenceImpl},
 *       {@code SQLReferenceImpl}, {@code QueryReferenceImpl} — named references to the
 *       above definitions, resolved lazily from the repository.
 *   <li>{@code AbstractMetaDataQueryColumn} — base for projected and content column
 *       implementations.
 *   <li>{@code MetaDataQueryProjectedColumnImpl} — a column that projects a bean
 *       attribute through binding; implements sorting, filtering, width, and format.
 *   <li>{@code MetaDataQueryContentColumnImpl} — a column that renders a content
 *       attachment thumbnail.
 * </ul>
 *
 * @see org.skyve.metadata.module.query
 */
package org.skyve.impl.metadata.module.query;
