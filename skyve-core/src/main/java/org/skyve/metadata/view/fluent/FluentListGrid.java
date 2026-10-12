package org.skyve.metadata.view.fluent;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds {@link ListGrid} widget metadata using a fluent API.
 */
public class FluentListGrid extends FluentWidget implements FluentRelativeSize<FluentListGrid> {
	private ListGrid grid = null;

	/**
	 * Creates a fluent builder backed by a new {@link ListGrid}.
	 */
	public FluentListGrid() {
		grid = new ListGrid();
	}

	/**
	 * Creates a fluent builder backed by the supplied {@link ListGrid}.
	 *
	 * @param grid
	 *            the metadata instance to mutate
	 */
	public FluentListGrid(ListGrid grid) {
		this.grid = grid;
	}

	/**
	 * Copies all list-grid configuration into this builder, including display options,
	 * filters, parameters, and event actions.
	 *
	 * @param grid
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentListGrid from(@SuppressWarnings("hiding") ListGrid grid) {
		title(grid.getTitle());
		escapeTitle(grid.getEscapeTitle());
		queryName(grid.getQueryName());
		modelName(grid.getModelName());
		postRefreshCondition(grid.getPostRefreshCondition());
		BindUtil.copyAuthoredCondition(grid.getXmlInvisibleCondition(), grid.getXmlVisibleCondition(), this::invisibleCondition, this::visibleCondition);
		grid.getFilterParameters();
		grid.getParameters();
		
		relativeSize(grid, this);

		grid.getParameters().forEach(p -> addParameter(new FluentParameter().from(p)));

		grid.getFilterParameters().forEach(f -> addFilterParameter(new FluentFilterParameter().from(f)));

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

	/**
	 * Sets the title for this list grid.
	 *
	 * @param title
	 *            the title text
	 * @return this builder
	 */
	public FluentListGrid title(String title) {
		grid.setTitle(title);
		return this;
	}

	/**
	 * Sets whether the list title should be escaped before rendering.
	 *
	 * @param escapeTitle {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentListGrid escapeTitle(boolean escapeTitle) {
		return escapeTitle(escapeTitle ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the list title should be escaped before rendering.
	 *
	 * @param escapeTitle {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentListGrid escapeTitle(Boolean escapeTitle) {
		grid.setEscapeTitle(escapeTitle);
		return this;
	}

	/**
	 * Sets the query name used to populate this list grid.
	 *
	 * @param queryName
	 *            the query name
	 * @return this builder
	 */
	public FluentListGrid queryName(String queryName) {
		grid.setQueryName(queryName);
		return this;
	}
	
	/**
	 * Sets the model name used to populate this list grid.
	 *
	 * @param modelName
	 *            the model name
	 * @return this builder
	 */
	public FluentListGrid modelName(String modelName) {
		grid.setModelName(modelName);
		return this;
	}
	
	/**
	 * Sets the {@code postRefresh} condition as authored.
	 *
	 * @param postRefreshCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentListGrid postRefreshCondition(@Nullable String postRefreshCondition) {
		grid.setPostRefreshCondition(postRefreshCondition);
		return this;
	}

	/**
	 * Appends a filter parameter to the list-grid query.
	 *
	 * @param filterParameter
	 *            the filter parameter to append
	 * @return this builder
	 */
	public FluentListGrid addFilterParameter(FluentFilterParameter filterParameter) {
		grid.getFilterParameters().add(filterParameter.get());
		return this;
	}

	/**
	 * Appends a query parameter to the list-grid query.
	 *
	 * @param parameter
	 *            the parameter to append
	 * @return this builder
	 */
	public FluentListGrid addParameter(FluentParameter parameter) {
		grid.getParameters().add(parameter.get());
		return this;
	}

	/**
	 * Sets the {@code invisible} condition as authored; the opposite sense of {@link #visibleCondition(String)}.
	 *
	 * @param invisibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentListGrid invisibleCondition(@Nullable String invisibleCondition) {
		grid.setXmlInvisibleCondition(invisibleCondition);
		return this;
	}

	/**
	 * Sets the {@code visible} condition as authored; the opposite sense of {@link #invisibleCondition(String)}.
	 *
	 * @param visibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentListGrid visibleCondition(@Nullable String visibleCondition) {
		grid.setXmlVisibleCondition(visibleCondition);
		return this;
	}

	/**
	 * Sets the pixel height of this list grid.
	 *
	 * @param height
	 *            the pixel height
	 * @return this builder
	 */
	@Override
	public FluentListGrid pixelHeight(int height) {
		grid.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Sets the pixel width of this list grid.
	 *
	 * @param width
	 *            the pixel width
	 * @return this builder
	 */
	@Override
	public FluentListGrid pixelWidth(int width) {
		grid.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the minimum pixel width of this list grid.
	 *
	 * @param minPixelWidth
	 *            the minimum pixel width
	 * @return this builder
	 */
	@Override
	public FluentListGrid minPixelWidth(int minPixelWidth) {
		grid.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel width of this list grid.
	 *
	 * @param maxPixelWidth
	 *            the maximum pixel width
	 * @return this builder
	 */
	@Override
	public FluentListGrid maxPixelWidth(int maxPixelWidth) {
		grid.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel height of this list grid.
	 *
	 * @param maxPixelHeight
	 *            the maximum pixel height
	 * @return this builder
	 */
	@Override
	public FluentListGrid maxPixelHeight(int maxPixelHeight) {
		grid.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	/**
	 * Sets the minimum pixel height of this list grid.
	 *
	 * @param minPixelHeight
	 *            the minimum pixel height
	 * @return this builder
	 */
	@Override
	public FluentListGrid minPixelHeight(int minPixelHeight) {
		grid.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the percentage width of this list grid.
	 *
	 * @param percentageWidth
	 *            the percentage width
	 * @return this builder
	 */
	@Override
	public FluentListGrid percentageWidth(int percentageWidth) {
		grid.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	/**
	 * Sets the responsive width breakpoint for this list grid.
	 *
	 * @param responsiveWidth
	 *            the responsive width
	 * @return this builder
	 */
	@Override
	public FluentListGrid responsiveWidth(int responsiveWidth) {
		grid.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	/**
	 * Sets the small breakpoint column span for this list grid.
	 *
	 * @param sm
	 *            the small breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentListGrid sm(int sm) {
		grid.setSm(Integer.valueOf(sm));
		return this;
	}

	/**
	 * Sets the medium breakpoint column span for this list grid.
	 *
	 * @param md
	 *            the medium breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentListGrid md(int md) {
		grid.setMd(Integer.valueOf(md));
		return this;
	}

	/**
	 * Sets the large breakpoint column span for this list grid.
	 *
	 * @param lg
	 *            the large breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentListGrid lg(int lg) {
		grid.setLg(Integer.valueOf(lg));
		return this;
	}

	/**
	 * Sets the extra-large breakpoint column span for this list grid.
	 *
	 * @param xl
	 *            the extra-large breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentListGrid xl(int xl) {
		grid.setXl(Integer.valueOf(xl));
		return this;
	}

	/**
	 * Sets the percentage height of this list grid.
	 *
	 * @param percentageHeight
	 *            the percentage height
	 * @return this builder
	 */
	@Override
	public FluentListGrid percentageHeight(int percentageHeight) {
		grid.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	/**
	 * Sets the {@code disabled} condition as authored; the opposite sense of {@link #enabledCondition(String)}.
	 *
	 * @param disabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentListGrid disabledCondition(@Nullable String disabledCondition) {
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
	public @Nonnull FluentListGrid enabledCondition(@Nullable String enabledCondition) {
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
	public @Nonnull FluentListGrid disableAddCondition(@Nullable String disableAddCondition) {
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
	public @Nonnull FluentListGrid enableAddCondition(@Nullable String enableAddCondition) {
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
	public @Nonnull FluentListGrid disableZoomCondition(@Nullable String disableZoomCondition) {
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
	public @Nonnull FluentListGrid enableZoomCondition(@Nullable String enableZoomCondition) {
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
	public @Nonnull FluentListGrid disableEditCondition(@Nullable String disableEditCondition) {
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
	public @Nonnull FluentListGrid enableEditCondition(@Nullable String enableEditCondition) {
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
	public @Nonnull FluentListGrid disableRemoveCondition(@Nullable String disableRemoveCondition) {
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
	public @Nonnull FluentListGrid enableRemoveCondition(@Nullable String enableRemoveCondition) {
		grid.setXmlEnableRemoveCondition(enableRemoveCondition);
		return this;
	}

	/**
	 * Sets the binding used to store the selected row identifier.
	 *
	 * @param selectedIdBinding
	 *            the binding expression
	 * @return this builder
	 */
	public FluentListGrid selectedIdBinding(String selectedIdBinding) {
		grid.setSelectedIdBinding(selectedIdBinding);
		return this;
	}

	/**
	 * Sets whether the add button is shown on this list grid.
	 *
	 * @param showAdd
	 *            {@code true} to show the add button
	 * @return this builder
	 */
	public FluentListGrid showAdd(boolean showAdd) {
		grid.setShowAdd(showAdd ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the zoom button is shown on this list grid.
	 *
	 * @param showZoom
	 *            {@code true} to show the zoom button
	 * @return this builder
	 */
	public FluentListGrid showZoom(boolean showZoom) {
		grid.setShowZoom(showZoom ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the edit button is shown on this list grid.
	 *
	 * @param showEdit
	 *            {@code true} to show the edit button
	 * @return this builder
	 */
	public FluentListGrid showEdit(boolean showEdit) {
		grid.setShowEdit(showEdit ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the remove button is shown on this list grid.
	 *
	 * @param showRemove
	 *            {@code true} to show the remove button
	 * @return this builder
	 */
	public FluentListGrid showRemove(boolean showRemove) {
		grid.setShowRemove(showRemove ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the deselect button is shown on this list grid.
	 *
	 * @param showDeselect
	 *            {@code true} to show the deselect button
	 * @return this builder
	 */
	public FluentListGrid showDeselect(boolean showDeselect) {
		grid.setShowDeselect(showDeselect ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the export button is shown on this list grid.
	 *
	 * @param showExport
	 *            {@code true} to show the export button
	 * @return this builder
	 */
	public FluentListGrid showExport(boolean showExport) {
		grid.setShowExport(showExport ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the chart button is shown on this list grid.
	 *
	 * @param showChart
	 *            {@code true} to show the chart button
	 * @return this builder
	 */
	public FluentListGrid showChart(boolean showChart) {
		grid.setShowChart(showChart ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the filter button is shown on this list grid.
	 *
	 * @param showFilter
	 *            {@code true} to show the filter button
	 * @return this builder
	 */
	public FluentListGrid showFilter(boolean showFilter) {
		grid.setShowFilter(showFilter ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the summary row is shown on this list grid.
	 *
	 * @param showSummary
	 *            {@code true} to show the summary row
	 * @return this builder
	 */
	public FluentListGrid showSummary(boolean showSummary) {
		grid.setShowSummary(showSummary ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the snap button is shown on this list grid.
	 *
	 * @param showSnap
	 *            {@code true} to show the snap button
	 * @return this builder
	 */
	public FluentListGrid showSnap(boolean showSnap) {
		grid.setShowSnap(showSnap ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the tag button is shown on this list grid.
	 *
	 * @param showTag
	 *            {@code true} to show the tag button
	 * @return this builder
	 */
	public FluentListGrid showTag(boolean showTag) {
		grid.setShowTag(showTag ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether the flag button is shown on this list grid.
	 *
	 * @param showFlag
	 *            {@code true} to show the flag button
	 * @return this builder
	 */
	public FluentListGrid showFlag(boolean showFlag) {
		grid.setShowFlag(showFlag ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether this list grid auto-populates on load.
	 *
	 * @param autoPopulate
	 *            {@code true} to auto-populate
	 * @return this builder
	 */
	public FluentListGrid autoPopulate(boolean autoPopulate) {
		grid.setAutoPopulate(autoPopulate ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether this list grid continues the current conversation context.
	 *
	 * @param continueConversation
	 *            {@code true} to continue the conversation
	 * @return this builder
	 */
	public FluentListGrid continueConversation(boolean continueConversation) {
		grid.setContinueConversation(continueConversation);
		return this;
	}

	/**
	 * Appends an action to invoke when a row is edited in this list grid.
	 *
	 * @param action
	 *            the event action to append
	 * @return this builder
	 */
	public FluentListGrid addEditedAction(FluentEventAction action) {
		grid.getEditedActions().add(action.get());
		return this;
	}

	/**
	 * Appends an action to invoke when a row is removed from this list grid.
	 *
	 * @param action
	 *            the event action to append
	 * @return this builder
	 */
	public FluentListGrid addRemovedAction(FluentEventAction action) {
		grid.getRemovedActions().add(action.get());
		return this;
	}

	/**
	 * Appends an action to invoke when a row is selected in this list grid.
	 *
	 * @param action
	 *            the event action to append
	 * @return this builder
	 */
	public FluentListGrid addSelectedAction(FluentEventAction action) {
		grid.getSelectedActions().add(action.get());
		return this;
	}

	/**
	 * Returns the wrapped {@link ListGrid} metadata instance.
	 *
	 * @return the mutable list-grid metadata being configured
	 */
	@Override
	public ListGrid get() {
		return grid;
	}
}
