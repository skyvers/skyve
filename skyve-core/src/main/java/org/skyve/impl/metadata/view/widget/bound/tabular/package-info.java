/**
 * Tabular bound widget implementations for displaying collections of domain objects.
 *
 * <p>This package provides the data-grid and list-grid widget hierarchy:
 * <ul>
 *   <li>{@code AbstractDataWidget} — base class for widgets driven by a collection
 *       binding (e.g. a {@code collection} or a query).
 *   <li>{@code AbstractListWidget} — base class for query-driven list widgets.
 *   <li>{@code DataGrid} — an editable grid bound to a collection attribute, with
 *       support for inline editing, adding, and removing rows.
 *   <li>{@code DataGridColumn} / {@code DataGridBoundColumn} /
 *       {@code DataGridContainerColumn} — column types for data grids.
 *   <li>{@code DataRepeater} — a read-only repeating layout for a collection, without
 *       grid chrome.
 *   <li>{@code DisableableCRUDGrid} — a data grid with configurable CRUD button
 *       visibility and disabled state.
 *   <li>{@code ListGrid} — a query-driven grid with sorting, filtering, and pagination.
 *   <li>{@code ListRepeater} — a read-only repeating layout driven by a query.
 *   <li>{@code TreeGrid} — a hierarchical tree-grid for documents with parent/child
 *       relationships.
 *   <li>{@code TabularColumn} — abstract column base used by both data and list grids.
 * </ul>
 *
 * @see org.skyve.impl.metadata.view.widget.bound
 * @see org.skyve.metadata.view.widget.bound.tabular
 */
package org.skyve.impl.metadata.view.widget.bound.tabular;
