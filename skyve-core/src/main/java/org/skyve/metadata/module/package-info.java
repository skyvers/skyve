/**
 * Module metadata contracts — the logical grouping unit for documents, queries, roles, and menus.
 *
 * <p>A <em>module</em> is the second tier in the Skyve metadata hierarchy
 * (customer &gt; module &gt; document). It groups related documents and exposes them
 * through a menu and a set of named queries and roles.
 *
 * <h2>Key interfaces</h2>
 * <ul>
 *   <li>{@link org.skyve.metadata.module.Module} — the top-level module contract. Provides
 *       access to documents, queries (BizQL, SQL, MetaData), jobs, roles, and the module
 *       menu. Also carries metadata-reload state via
 *       {@link org.skyve.metadata.PersistentMetaData} and
 *       {@link org.skyve.metadata.ReloadableMetaData}.</li>
 *   <li>{@link org.skyve.metadata.module.Module.DocumentRef} — a cross-reference descriptor
 *       that records the module in which a document is actually defined, enabling documents
 *       to be shared across module boundaries.</li>
 *   <li>{@link org.skyve.metadata.module.JobMetaData} — metadata for a background job
 *       declared in a module, linking its display name, description, and implementation
 *       class.</li>
 * </ul>
 *
 * <h2>Sub-packages</h2>
 * <ul>
 *   <li>{@code org.skyve.metadata.module.menu} — menu item contracts (grouped, list, edit, etc.).</li>
 *   <li>{@code org.skyve.metadata.module.query} — query definition contracts
 *       ({@code MetaDataQueryDefinition}, {@code BizQLDefinition}, {@code SQLDefinition}).</li>
 *   <li>{@code org.skyve.metadata.module.fluent} — fluent builder API for constructing module
 *       metadata programmatically.</li>
 * </ul>
 */
package org.skyve.metadata.module;
