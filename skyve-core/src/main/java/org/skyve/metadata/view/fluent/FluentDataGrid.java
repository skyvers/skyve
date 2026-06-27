package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;

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

		disabledConditionName(grid.getDisabledConditionName());
		disableAddConditionName(grid.getDisableAddConditionName());
		disableZoomConditionName(grid.getDisableZoomConditionName());
		disableEditConditionName(grid.getDisableEditConditionName());
		disableRemoveConditionName(grid.getDisableRemoveConditionName());
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
	 * Sets the condition name that disables this data grid.
	 *
	 * @param disabledConditionName the condition name
	 * @return this builder
	 */
	public FluentDataGrid disabledConditionName(String disabledConditionName) {
		grid.setDisabledConditionName(disabledConditionName);
		return this;
	}

	/**
	 * Sets the condition name that disables the add action on this data grid.
	 *
	 * @param disableAddConditionName the condition name
	 * @return this builder
	 */
	public FluentDataGrid disableAddConditionName(String disableAddConditionName) {
		grid.setDisableAddConditionName(disableAddConditionName);
		return this;
	}

	/**
	 * Sets the condition name that disables the zoom action on this data grid.
	 *
	 * @param disableZoomConditionName the condition name
	 * @return this builder
	 */
	public FluentDataGrid disableZoomConditionName(String disableZoomConditionName) {
		grid.setDisableZoomConditionName(disableZoomConditionName);
		return this;
	}

	/**
	 * Sets the condition name that disables the edit action on this data grid.
	 *
	 * @param disableEditConditionName the condition name
	 * @return this builder
	 */
	public FluentDataGrid disableEditConditionName(String disableEditConditionName) {
		grid.setDisableEditConditionName(disableEditConditionName);
		return this;
	}

	/**
	 * Sets the condition name that disables the remove action on this data grid.
	 *
	 * @param disableRemoveConditionName the condition name
	 * @return this builder
	 */
	public FluentDataGrid disableRemoveConditionName(String disableRemoveConditionName) {
		grid.setDisableRemoveConditionName(disableRemoveConditionName);
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
