/**
 * SAIL steps for grid widget operations: data grids and list grids.
 *
 * <p>This package contains steps that simulate user interactions with Skyve grid widgets:
 * <ul>
 *   <li><b>Data-grid steps</b> — operate on embedded collection grids within an edit view:
 *     <ul>
 *       <li>{@link org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew} — adds a
 *           new row to the data grid bound to the specified binding.
 *       <li>{@link org.skyve.metadata.sail.language.step.interaction.grids.DataGridSelect} — selects
 *           a row by its zero-based index.
 *       <li>{@link org.skyve.metadata.sail.language.step.interaction.grids.DataGridEdit} — opens a
 *           row for inline editing.
 *       <li>{@link org.skyve.metadata.sail.language.step.interaction.grids.DataGridZoom} — zooms into
 *           a row, opening it in a modal or new edit view.
 *       <li>{@link org.skyve.metadata.sail.language.step.interaction.grids.DataGridRemove} — removes
 *           the row at the specified index.
 *       <li>{@link org.skyve.metadata.sail.language.step.interaction.grids.DataGridMove} — drags a row
 *           of an ordered data grid to another position.
 *       <li>{@link org.skyve.metadata.sail.language.step.interaction.grids.ListMembershipMove} — drags a
 *           member of an ordered list membership to another position in its member list.
 *     </ul>
 *   <li><b>List-grid steps</b> — operate on module-level list views:
 *     <ul>
 *       <li>{@link org.skyve.metadata.sail.language.step.interaction.grids.ListGridNew} — clicks the
 *           New button on a list grid.
 *       <li>{@link org.skyve.metadata.sail.language.step.interaction.grids.ListGridSelect} — selects
 *           a row by its zero-based index, e.g. for tag/untag operations.
 *       <li>{@link org.skyve.metadata.sail.language.step.interaction.grids.ListGridZoom} — zooms into
 *           a row by its zero-based index.
 *       <li>{@link org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter} — filters
 *           one column, or clears that column's filter.
 *       <li>{@link org.skyve.metadata.sail.language.step.interaction.grids.ListGridClearFilter} — clears
 *           every column filter.
 *       <li>{@link org.skyve.metadata.sail.language.step.interaction.grids.ListGridSort} — sorts by
 *           one column.
 *     </ul>
 * </ul>
 *
 * <p>The row steps implement {@link org.skyve.metadata.sail.language.step.interaction.grids.GridRowStep},
 * so a row can be located by index ({@code row}) or by the text of one of its cells ({@code rowContaining}).
 *
 * @see org.skyve.metadata.sail.language.step.interaction
 */
package org.skyve.metadata.sail.language.step.interaction.grids;
