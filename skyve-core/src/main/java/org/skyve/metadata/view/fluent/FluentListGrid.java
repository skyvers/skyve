package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;

public class FluentListGrid extends FluentWidget {
	private ListGrid grid = null;

	public FluentListGrid() {
		grid = new ListGrid();
	}

	public FluentListGrid(ListGrid grid) {
		this.grid = grid;
	}

	public FluentListGrid from(@SuppressWarnings("hiding") ListGrid grid) {
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
		b = grid.getShowExport();
		if (b != null) {
			showExport(b.booleanValue());
		}
		b = grid.getShowChart();
		if (b != null) {
			showChart(b.booleanValue());
		}
		b = grid.getShowFilter();
		if (b != null) {
			showFilter(b.booleanValue());
		}
		b = grid.getShowSummary();
		if (b != null) {
			showSummary(b.booleanValue());
		}
		b = grid.getShowSnap();
		if (b != null) {
			showSnap(b.booleanValue());
		}
		b = grid.getShowTag();
		if (b != null) {
			showTag(b.booleanValue());
		}
		b = grid.getAutoPopulate();
		if (b != null) {
			autoPopulate(b.booleanValue());
		}
		continueConversation(grid.getContinueConversation());
		selectedIdBinding(grid.getSelectedIdBinding());

		grid.getEditedActions().forEach(e -> addEditedAction(FluentEventAction.from(e)));

		grid.getRemovedActions().forEach(r -> addRemovedAction(FluentEventAction.from(r)));

		grid.getSelectedActions().forEach(s -> addSelectedAction(FluentEventAction.from(s)));

		return this;
	}

	public FluentListGrid disabledConditionName(String disabledConditionName) {
		grid.setDisabledConditionName(disabledConditionName);
		return this;
	}

	public FluentListGrid disableAddConditionName(String disableAddConditionName) {
		grid.setDisableAddConditionName(disableAddConditionName);
		return this;
	}

	public FluentListGrid disableZoomConditionName(String disableZoomConditionName) {
		grid.setDisableZoomConditionName(disableZoomConditionName);
		return this;
	}

	public FluentListGrid disableEditConditionName(String disableEditConditionName) {
		grid.setDisableEditConditionName(disableEditConditionName);
		return this;
	}

	public FluentListGrid disableRemoveConditionName(String disableRemoveConditionName) {
		grid.setDisableRemoveConditionName(disableRemoveConditionName);
		return this;
	}

	public FluentListGrid selectedIdBinding(String selectedIdBinding) {
		grid.setSelectedIdBinding(selectedIdBinding);
		return this;
	}

	public FluentListGrid showAdd(boolean showAdd) {
		grid.setShowAdd(showAdd ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListGrid showZoom(boolean showZoom) {
		grid.setShowZoom(showZoom ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListGrid showEdit(boolean showEdit) {
		grid.setShowEdit(showEdit ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListGrid showRemove(boolean showRemove) {
		grid.setShowRemove(showRemove ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListGrid showDeselect(boolean showDeselect) {
		grid.setShowDeselect(showDeselect ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListGrid showExport(boolean showExport) {
		grid.setShowExport(showExport ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListGrid showChart(boolean showChart) {
		grid.setShowChart(showChart ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListGrid showFilter(boolean showFilter) {
		grid.setShowFilter(showFilter ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListGrid showSummary(boolean showSummary) {
		grid.setShowSummary(showSummary ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListGrid showSnap(boolean showSnap) {
		grid.setShowSnap(showSnap ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListGrid showTag(boolean showTag) {
		grid.setShowTag(showTag ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListGrid autoPopulate(boolean autoPopulate) {
		grid.setAutoPopulate(autoPopulate ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListGrid continueConversation(boolean continueConversation) {
		grid.setContinueConversation(continueConversation);
		return this;
	}

	public FluentListGrid addEditedAction(FluentEventAction action) {
		grid.getEditedActions().add(action.get());
		return this;
	}

	public FluentListGrid addRemovedAction(FluentEventAction action) {
		grid.getRemovedActions().add(action.get());
		return this;
	}

	public FluentListGrid addSelectedAction(FluentEventAction action) {
		grid.getSelectedActions().add(action.get());
		return this;
	}

	@Override
	public ListGrid get() {
		return grid;
	}
}
