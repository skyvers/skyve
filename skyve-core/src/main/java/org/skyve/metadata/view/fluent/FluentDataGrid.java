package org.skyve.metadata.view.fluent;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds tabular {@link DataGrid} metadata for bound-data views.
 */
public class FluentDataGrid extends FluentDataWidget<FluentDataGrid> {
	private DataGrid grid = null;

	/**
	 * Creates a builder backed by a new {@link DataGrid}.
	 */
	public FluentDataGrid() {
		grid = new DataGrid();
	}

	/**
	 * Creates a builder backed by the supplied {@link DataGrid}.
	 *
	 * @param grid
	 *            the metadata instance to mutate
	 */
	public FluentDataGrid(DataGrid grid) {
		this.grid = grid;
	}

	/**
	 * Copies grid configuration, columns, and event actions from runtime metadata.
	 *
	 * @param grid
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentDataGrid from(@SuppressWarnings("hiding") DataGrid grid) {

		BindUtil.copyAuthoredCondition(grid.getXmlDisabledCondition(), grid.getXmlEnabledCondition(), this::disabledCondition, this::enabledCondition);
		BindUtil.copyAuthoredCondition(grid.getXmlDisableAddCondition(), grid.getXmlEnableAddCondition(), this::disableAddCondition, this::enableAddCondition);
		BindUtil.copyAuthoredCondition(grid.getXmlDisableZoomCondition(), grid.getXmlEnableZoomCondition(), this::disableZoomCondition, this::enableZoomCondition);
		BindUtil.copyAuthoredCondition(grid.getXmlDisableEditCondition(), grid.getXmlEnableEditCondition(), this::disableEditCondition, this::enableEditCondition);
		BindUtil.copyAuthoredCondition(grid.getXmlDisableRemoveCondition(), grid.getXmlEnableRemoveCondition(), this::disableRemoveCondition, this::enableRemoveCondition);
		Boolean b = grid.getShowAdd();
		if (b != null) {
			showAdd(b.booleanValue());
		}
		b = grid.getShowEdit();
		if (b != null) {
			showEdit(b.booleanValue());
		}
		b = grid.getShowZoom();
		if (b != null) {
			showZoom(b.booleanValue());
		}
		b = grid.getShowRemove();
		if (b != null) {
			showRemove(b.booleanValue());
		}
		b = grid.getShowDeselect();
		if (b != null) {
			showDeselect(b.booleanValue());
		}
		selectedIdBinding(grid.getSelectedIdBinding());
		b = grid.getInline();
		if (b != null) {
			inline(b.booleanValue());
		}
		b = grid.getEditable();
		if (b != null) {
			editable(b.booleanValue());
		}
		b = grid.getWordWrap();
		if (b != null) {
			wordWrap(b.booleanValue());
		}

		for (DataGridColumn column : grid.getColumns()) {
			if (column instanceof DataGridBoundColumn) {
				addBoundColumn(new FluentDataGridBoundColumn().from(column));
			}
			else {
				addContainerColumn(new FluentDataGridContainerColumn().from(column));
			}
		}

		grid.getAddedActions().forEach(a -> addAddedAction(FluentEventAction.from(a)));

		grid.getEditedActions().forEach(e -> addEditedAction(FluentEventAction.from(e)));

		grid.getRemovedActions().forEach(r -> addRemovedAction(FluentEventAction.from(r)));

		grid.getSelectedActions().forEach(s -> addSelectedAction(FluentEventAction.from(s)));

		super.from(grid);
		return this;
	}

	/**
	 * Sets the {@code disabled} condition as authored; the opposite sense of {@link #enabledCondition(String)}.
	 *
	 * @param disabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentDataGrid disabledCondition(@Nullable String disabledCondition) {
		grid.setXmlDisabledCondition(disabledCondition);
		return this;
	}

	/**
	 * Sets the {@code enabled} condition as authored; the opposite sense of {@link #disabledCondition(String)}.
	 *
	 * @param enabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentDataGrid enabledCondition(@Nullable String enabledCondition) {
		grid.setXmlEnabledCondition(enabledCondition);
		return this;
	}

	/**
	 * Sets the {@code disableAdd} condition as authored; the opposite sense of {@link #enableAddCondition(String)}.
	 *
	 * @param disableAddCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentDataGrid disableAddCondition(@Nullable String disableAddCondition) {
		grid.setXmlDisableAddCondition(disableAddCondition);
		return this;
	}

	/**
	 * Sets the {@code enableAdd} condition as authored; the opposite sense of {@link #disableAddCondition(String)}.
	 *
	 * @param enableAddCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentDataGrid enableAddCondition(@Nullable String enableAddCondition) {
		grid.setXmlEnableAddCondition(enableAddCondition);
		return this;
	}

	/**
	 * Sets the {@code disableZoom} condition as authored; the opposite sense of {@link #enableZoomCondition(String)}.
	 *
	 * @param disableZoomCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentDataGrid disableZoomCondition(@Nullable String disableZoomCondition) {
		grid.setXmlDisableZoomCondition(disableZoomCondition);
		return this;
	}

	/**
	 * Sets the {@code enableZoom} condition as authored; the opposite sense of {@link #disableZoomCondition(String)}.
	 *
	 * @param enableZoomCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentDataGrid enableZoomCondition(@Nullable String enableZoomCondition) {
		grid.setXmlEnableZoomCondition(enableZoomCondition);
		return this;
	}

	/**
	 * Sets the {@code disableEdit} condition as authored; the opposite sense of {@link #enableEditCondition(String)}.
	 *
	 * @param disableEditCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentDataGrid disableEditCondition(@Nullable String disableEditCondition) {
		grid.setXmlDisableEditCondition(disableEditCondition);
		return this;
	}

	/**
	 * Sets the {@code enableEdit} condition as authored; the opposite sense of {@link #disableEditCondition(String)}.
	 *
	 * @param enableEditCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentDataGrid enableEditCondition(@Nullable String enableEditCondition) {
		grid.setXmlEnableEditCondition(enableEditCondition);
		return this;
	}

	/**
	 * Sets the {@code disableRemove} condition as authored; the opposite sense of {@link #enableRemoveCondition(String)}.
	 *
	 * @param disableRemoveCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentDataGrid disableRemoveCondition(@Nullable String disableRemoveCondition) {
		grid.setXmlDisableRemoveCondition(disableRemoveCondition);
		return this;
	}

	/**
	 * Sets the {@code enableRemove} condition as authored; the opposite sense of {@link #disableRemoveCondition(String)}.
	 *
	 * @param enableRemoveCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentDataGrid enableRemoveCondition(@Nullable String enableRemoveCondition) {
		grid.setXmlEnableRemoveCondition(enableRemoveCondition);
		return this;
	}

	/**
	 * Sets whether the add button is shown on this data grid.
	 *
	 * @param showAdd {@code true} to show the add button
	 * @return this builder
	 */
	public FluentDataGrid showAdd(boolean showAdd) {
		grid.setShowAdd(showAdd ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the zoom button is shown on this data grid.
	 *
	 * @param showZoom {@code true} to show the zoom button
	 * @return this builder
	 */
	public FluentDataGrid showZoom(boolean showZoom) {
		grid.setShowZoom(showZoom ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the edit button is shown on this data grid.
	 *
	 * @param showEdit {@code true} to show the edit button
	 * @return this builder
	 */
	public FluentDataGrid showEdit(boolean showEdit) {
		grid.setShowEdit(showEdit ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the remove button is shown on this data grid.
	 *
	 * @param showRemove {@code true} to show the remove button
	 * @return this builder
	 */
	public FluentDataGrid showRemove(boolean showRemove) {
		grid.setShowRemove(showRemove ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the deselect button is shown on this data grid.
	 *
	 * @param showDeselect {@code true} to show the deselect button
	 * @return this builder
	 */
	public FluentDataGrid showDeselect(boolean showDeselect) {
		grid.setShowDeselect(showDeselect ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the binding used to store the selected row identifier.
	 *
	 * @param selectedIdBinding the binding expression
	 * @return this builder
	 */
	public FluentDataGrid selectedIdBinding(String selectedIdBinding) {
		grid.setSelectedIdBinding(selectedIdBinding);
		return this;
	}

	/**
	 * Sets whether rows in this data grid are edited inline.
	 *
	 * @param inline {@code true} to enable inline editing
	 * @return this builder
	 */
	public FluentDataGrid inline(boolean inline) {
		grid.setInline(inline ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether this data grid is editable.
	 *
	 * @param editable {@code true} to allow editing
	 * @return this builder
	 */
	public FluentDataGrid editable(boolean editable) {
		grid.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether text in this data grid wraps.
	 *
	 * @param wordWrap {@code true} to enable word wrap
	 * @return this builder
	 */
	public FluentDataGrid wordWrap(boolean wordWrap) {
		grid.setWordWrap(wordWrap ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Appends a bound column to the column list.
	 *
	 * @param column
	 *            the bound column to append
	 * @return this builder
	 */
	public FluentDataGrid addBoundColumn(FluentDataGridBoundColumn column) {
		grid.getColumns().add(column.get());
		return this;
	}

	/**
	 * Inserts a bound column at the specified position.
	 *
	 * @param index
	 *            insertion index in the grid column list
	 * @param column
	 *            the bound column to insert
	 * @return this builder
	 */
	public FluentDataGrid addBoundColumn(int index, FluentDataGridBoundColumn column) {
		grid.getColumns().add(index, column.get());
		return this;
	}

	/**
	 * Returns a fluent wrapper for a bound column at the specified index.
	 *
	 * @param index
	 *            the column position
	 * @return a fluent wrapper around the bound column
	 */
	public FluentDataGridBoundColumn getBoundColumn(int index) {
		return new FluentDataGridBoundColumn((DataGridBoundColumn) grid.getColumns().get(index));
	}
	
	/**
	 * Appends a container column to the column list.
	 *
	 * @param column
	 *            the container column to append
	 * @return this builder
	 */
	public FluentDataGrid addContainerColumn(FluentDataGridContainerColumn column) {
		grid.getColumns().add(column.get());
		return this;
	}

	/**
	 * Inserts a container column at the specified position.
	 *
	 * @param index
	 *            insertion index in the grid column list
	 * @param column
	 *            the container column to insert
	 * @return this builder
	 */
	public FluentDataGrid addContainerColumn(int index, FluentDataGridContainerColumn column) {
		grid.getColumns().add(index, column.get());
		return this;
	}

	/**
	 * Returns a fluent wrapper for a container column at the specified index.
	 *
	 * @param index
	 *            the column position
	 * @return a fluent wrapper around the container column
	 */
	public FluentDataGridContainerColumn getContainerColumn(int index) {
		return new FluentDataGridContainerColumn((DataGridContainerColumn) grid.getColumns().get(index));
	}

	/**
	 * Removes the column at the specified index.
	 *
	 * @param index
	 *            the column position to remove
	 * @return this builder
	 */
	public FluentDataGrid removeColumn(int index) {
		grid.getColumns().remove(index);
		return this;
	}
	
	/**
	 * Clears all configured columns.
	 *
	 * @return this builder
	 */
	public FluentDataGrid clearColumns() {
		grid.getColumns().clear();
		return this;
	}
	
	/**
	 * Appends an added-event action.
	 *
	 * @param action
	 *            the action to append
	 * @return this builder
	 */
	public FluentDataGrid addAddedAction(FluentEventAction action) {
		grid.getAddedActions().add(action.get());
		return this;
	}

	/**
	 * Clears added-event actions.
	 *
	 * @return this builder
	 */
	public FluentDataGrid clearAddedActions() {
		grid.getAddedActions().clear();
		return this;
	}

	/**
	 * Appends an edited-row action.
	 *
	 * @param action the action to append
	 * @return this builder
	 */
	public FluentDataGrid addEditedAction(FluentEventAction action) {
		grid.getEditedActions().add(action.get());
		return this;
	}

	/**
	 * Clears edited-event actions.
	 *
	 * @return this builder
	 */
	public FluentDataGrid clearEditedActions() {
		grid.getEditedActions().clear();
		return this;
	}

	/**
	 * Appends a removed-row action.
	 *
	 * @param action the action to append
	 * @return this builder
	 */
	public FluentDataGrid addRemovedAction(FluentEventAction action) {
		grid.getRemovedActions().add(action.get());
		return this;
	}

	/**
	 * Clears removed-event actions.
	 *
	 * @return this builder
	 */
	public FluentDataGrid clearRemovedActions() {
		grid.getRemovedActions().clear();
		return this;
	}

	/**
	 * Appends a selected-row action.
	 *
	 * @param action the action to append
	 * @return this builder
	 */
	public FluentDataGrid addSelectedAction(FluentEventAction action) {
		grid.getSelectedActions().add(action.get());
		return this;
	}

	/**
	 * Clears selected-event actions.
	 *
	 * @return this builder
	 */
	public FluentDataGrid clearSelectedActions() {
		grid.getSelectedActions().clear();
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped data-grid metadata
	 */
	@Override
	public DataGrid get() {
		return grid;
	}
}
