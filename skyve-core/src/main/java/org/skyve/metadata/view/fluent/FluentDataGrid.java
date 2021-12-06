package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;

public class FluentDataGrid extends FluentDataWidget<FluentDataGrid> {
	private DataGrid grid = null;

	public FluentDataGrid() {
		grid = new DataGrid();
	}

	public FluentDataGrid(DataGrid grid) {
		this.grid = grid;
	}

	public FluentDataGrid from(@SuppressWarnings("hiding") DataGrid grid) {

		disabledConditionName(grid.getDisabledConditionName());
		disableAddConditionName(grid.getDisableAddConditionName());
		disableZoomConditionName(grid.getDisableZoomConditionName());
		disableEditConditionName(grid.getDisableEditConditionName());
		disableRemoveConditionName(grid.getDisableRemoveConditionName());
		showAdd(grid.getShowAdd());
		showEdit(grid.getShowEdit());
		showZoom(grid.getShowZoom());
		showRemove(grid.getShowRemove());
		showDeselect(grid.getShowDeselect());
		selectedIdBinding(grid.getSelectedIdBinding());
		inline(grid.getInline());
		editable(grid.getEditable());
		wordWrap(grid.getWordWrap());

		// grid.getColumns().forEach(c -> addColumn(FluentDataGridColumn.from(c)));

		grid.getAddedActions().forEach(a -> addAddedAction(FluentEventAction.from(a)));
		grid.getEditedActions().forEach(e -> addEditedAction(FluentEventAction.from(e)));
		grid.getRemovedActions().forEach(r -> addRemovedAction(FluentEventAction.from(r)));
		grid.getSelectedActions().forEach(s -> addSelectedAction(FluentEventAction.from(s)));

		super.from(grid);
		return this;
	}

	public FluentDataGrid disabledConditionName(String disabledConditionName) {
		grid.setDisabledConditionName(disabledConditionName);
		return this;
	}

	public FluentDataGrid disableAddConditionName(String disableAddConditionName) {
		grid.setDisableAddConditionName(disableAddConditionName);
		return this;
	}

	public FluentDataGrid disableZoomConditionName(String disableZoomConditionName) {
		grid.setDisableZoomConditionName(disableZoomConditionName);
		return this;
	}

	public FluentDataGrid disableEditConditionName(String disableEditConditionName) {
		grid.setDisableEditConditionName(disableEditConditionName);
		return this;
	}

	public FluentDataGrid disableRemoveConditionName(String disableRemoveConditionName) {
		grid.setDisableRemoveConditionName(disableRemoveConditionName);
		return this;
	}

	public FluentDataGrid showAdd(Boolean showAdd) {
		grid.setShowAdd(showAdd);
		return this;
	}

	public FluentDataGrid showZoom(Boolean showZoom) {
		grid.setShowZoom(showZoom);
		return this;
	}

	public FluentDataGrid showEdit(Boolean showEdit) {
		grid.setShowEdit(Boolean.valueOf(showEdit));
		return this;
	}

	public FluentDataGrid showRemove(Boolean showRemove) {
		grid.setShowRemove(showRemove);
		return this;
	}

	public FluentDataGrid showDeselect(Boolean showDeselect) {
		grid.setShowDeselect(showDeselect);
		return this;
	}

	public FluentDataGrid selectedIdBinding(String selectedIdBinding) {
		grid.setSelectedIdBinding(selectedIdBinding);
		return this;
	}

	public FluentDataGrid inline(Boolean inline) {
		grid.setInline(inline);
		return this;
	}

	public FluentDataGrid editable(Boolean editable) {
		grid.setEditable(editable);
		return this;
	}

	public FluentDataGrid wordWrap(Boolean wordWrap) {
		grid.setWordWrap(wordWrap);
		return this;
	}

	// public FluentDataGrid addColumn( FluentDataGridColumn column) {
	// grid.getColumns().add(column.get());
	// return this;
	// }

	public FluentDataGrid addAddedAction(FluentEventAction action) {
		grid.getAddedActions().add(action.get());
		return this;
	}

	public FluentDataGrid addEditedAction(FluentEventAction action) {
		grid.getEditedActions().add(action.get());
		return this;
	}

	public FluentDataGrid addRemovedAction(FluentEventAction action) {
		grid.getRemovedActions().add(action.get());
		return this;
	}

	public FluentDataGrid addSelectedAction(FluentEventAction action) {
		grid.getSelectedActions().add(action.get());
		return this;
	}

	@Override
	public DataGrid get() {
		return grid;
	}
}
