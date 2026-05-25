/**
 * Defines the query model used by Skyve list views and programmatic query execution.
 *
 * <p>Skyve supports three query flavours, all extending {@link org.skyve.metadata.module.query.QueryDefinition}:
 * <ul>
 *   <li>{@link org.skyve.metadata.module.query.MetaDataQueryDefinition} &mdash; the primary
 *       document-driven query that powers list views, calendar views, map views, and tree views.
 *       It carries an ordered set of {@link org.skyve.metadata.module.query.MetaDataQueryColumn}
 *       projections and optional SQL fragment overlays.</li>
 *   <li>{@link org.skyve.metadata.module.query.BizQLDefinition} &mdash; a query expressed in
 *       Skyve BizQL (an HQL dialect), executed against the ORM layer.</li>
 *   <li>{@link org.skyve.metadata.module.query.SQLDefinition} &mdash; a native SQL query executed
 *       directly against the database.</li>
 * </ul>
 *
 * <p>All query definitions are declared in module XML metadata, loaded by the metadata
 * repository, and resolved at runtime via
 * {@link org.skyve.metadata.module.Module#getMetaDataQuery} and related methods.
 *
 * <p>Column metadata for {@code MetaDataQueryDefinition} is further specialised into:
 * <ul>
 *   <li>{@link org.skyve.metadata.module.query.MetaDataQueryProjectedColumn} &mdash; a
 *       binding- or expression-based column with sortable, filterable, and editable flags.</li>
 *   <li>{@link org.skyve.metadata.module.query.MetaDataQueryContentColumn} &mdash; a column
 *       that renders a content attachment as a thumbnail or link.</li>
 * </ul>
 *
 * @see org.skyve.metadata.module.query.QueryDefinition
 * @see org.skyve.metadata.module.query.MetaDataQueryDefinition
 * @see org.skyve.metadata.module.Module
 */
package org.skyve.metadata.module.query;
