package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;

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

	public FluentDataGrid showAdd(boolean showAdd) {
		grid.setShowAdd(showAdd ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentDataGrid showZoom(boolean showZoom) {
		grid.setShowZoom(showZoom ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentDataGrid showEdit(boolean showEdit) {
		grid.setShowEdit(showEdit ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentDataGrid showRemove(boolean showRemove) {
		grid.setShowRemove(showRemove ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentDataGrid showDeselect(boolean showDeselect) {
		grid.setShowDeselect(showDeselect ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentDataGrid selectedIdBinding(String selectedIdBinding) {
		grid.setSelectedIdBinding(selectedIdBinding);
		return this;
	}

	public FluentDataGrid inline(boolean inline) {
		grid.setInline(inline ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentDataGrid editable(boolean editable) {
		grid.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentDataGrid wordWrap(boolean wordWrap) {
		grid.setWordWrap(wordWrap ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentDataGrid addBoundColumn(FluentDataGridBoundColumn column) {
		grid.getColumns().add(column.get());
		return this;
	}

	public FluentDataGrid addContainerColumn(FluentDataGridContainerColumn column) {
		grid.getColumns().add(column.get());
		return this;
	}

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
