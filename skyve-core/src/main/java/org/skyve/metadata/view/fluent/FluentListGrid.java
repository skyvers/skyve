package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;

public class FluentListGrid extends FluentWidget implements FluentRelativeSize<FluentListGrid> {
	private ListGrid grid = null;

	public FluentListGrid() {
		grid = new ListGrid();
	}

	public FluentListGrid(ListGrid grid) {
		this.grid = grid;
	}

	public FluentListGrid from(@SuppressWarnings("hiding") ListGrid grid) {
		title(grid.getTitle());
		queryName(grid.getQueryName());
		modelName(grid.getModelName());
		postRefreshConditionName(grid.getPostRefreshConditionName());
		grid.getFilterParameters();
		grid.getParameters();
		
		relativeSize(grid, this);

		grid.getParameters().forEach(p -> addParameter(new FluentParameter().from(p)));

		grid.getFilterParameters().forEach(f -> addFilterParameter(new FluentFilterParameter().from(f)));

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
		b = grid.getShowFlag();
		if (b != null) {
			showFlag(b.booleanValue());
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

	public FluentListGrid title(String title) {
		grid.setTitle(title);
		return this;
	}
	
	public FluentListGrid queryName(String queryName) {
		grid.setQueryName(queryName);
		return this;
	}
	
	public FluentListGrid modelName(String modelName) {
		grid.setModelName(modelName);
		return this;
	}
	
	public FluentListGrid postRefreshConditionName(String postRefreshConditionName) {
		grid.setPostRefreshConditionName(postRefreshConditionName);
		return this;
	}

	public FluentListGrid addFilterParameter(FluentFilterParameter filterParameter) {
		grid.getFilterParameters().add(filterParameter.get());
		return this;
	}

	public FluentListGrid addParameter(FluentParameter parameter) {
		grid.getParameters().add(parameter.get());
		return this;
	}

	public FluentListGrid invisibleConditionName(String invisibleConditionName) {
		grid.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	@Override
	public FluentListGrid pixelHeight(int height) {
		grid.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public FluentListGrid pixelWidth(int width) {
		grid.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentListGrid minPixelWidth(int minPixelWidth) {
		grid.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	@Override
	public FluentListGrid maxPixelWidth(int maxPixelWidth) {
		grid.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	@Override
	public FluentListGrid maxPixelHeight(int maxPixelHeight) {
		grid.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	@Override
	public FluentListGrid minPixelHeight(int minPixelHeight) {
		grid.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentListGrid percentageWidth(int percentageWidth) {
		grid.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	@Override
	public FluentListGrid responsiveWidth(int responsiveWidth) {
		grid.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	@Override
	public FluentListGrid sm(int sm) {
		grid.setSm(Integer.valueOf(sm));
		return this;
	}

	@Override
	public FluentListGrid md(int md) {
		grid.setMd(Integer.valueOf(md));
		return this;
	}

	@Override
	public FluentListGrid lg(int lg) {
		grid.setLg(Integer.valueOf(lg));
		return this;
	}

	@Override
	public FluentListGrid xl(int xl) {
		grid.setXl(Integer.valueOf(xl));
		return this;
	}

	@Override
	public FluentListGrid percentageHeight(int percentageHeight) {
		grid.setPercentageHeight(Integer.valueOf(percentageHeight));
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

	public FluentListGrid showFlag(boolean showFlag) {
		grid.setShowFlag(showFlag ? Boolean.TRUE : Boolean.FALSE);
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
