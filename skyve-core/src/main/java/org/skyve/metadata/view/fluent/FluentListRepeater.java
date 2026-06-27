package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;

/**
 * Builds {@link ListRepeater} metadata for repeated list-query rendering.
 */
public class FluentListRepeater extends FluentWidget implements FluentRelativeSize<FluentListRepeater> {
	private ListRepeater list = null;

	/**
	 * Creates a builder backed by a new {@link ListRepeater}.
	 */
	public FluentListRepeater() {
		list = new ListRepeater();
	}

	/**
	 * Creates a builder backed by the supplied {@link ListRepeater}.
	 *
	 * @param list
	 *            the metadata instance to mutate
	 */
	public FluentListRepeater(ListRepeater list) {
		this.list = list;
	}

	/**
	 * Copies list-repeater display configuration, filters, and parameters.
	 *
	 * @param list
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentListRepeater from(@SuppressWarnings("hiding") ListRepeater list) {
		Boolean b = list.getShowColumnHeaders();
		if (b != null) {
			showColumnHeaders(b.booleanValue());
		}
		b = list.getShowGrid();
		if (b != null) {
			showGrid(b.booleanValue());
		}

		title(list.getTitle());
		escapeTitle(list.getEscapeTitle());
		queryName(list.getQueryName());
		modelName(list.getModelName());
		postRefreshConditionName(list.getPostRefreshConditionName());
		invisibleConditionName(list.getInvisibleConditionName());

		relativeSize(list, this);

		list.getParameters().forEach(p -> addParameter(new FluentParameter().from(p)));

		list.getFilterParameters().forEach(f -> addFilterParameter(new FluentFilterParameter().from(f)));

		return this;
	}

	/**
	 * Sets whether column headers are shown.
	 *
	 * @param showColumnHeaders
	 *            {@code true} to show headers
	 * @return this builder
	 */
	public FluentListRepeater showColumnHeaders(boolean showColumnHeaders) {
		list.setShowColumnHeaders(showColumnHeaders ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether grid styling is shown.
	 *
	 * @param showGrid
	 *            {@code true} to show grid styling
	 * @return this builder
	 */
	public FluentListRepeater showGrid(boolean showGrid) {
		list.setShowGrid(showGrid ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the list-repeater title.
	 *
	 * @param title
	 *            the title shown for the repeater
	 * @return this builder
	 */
	public FluentListRepeater title(String title) {
		list.setTitle(title);
		return this;
	}

	/**
	 * Sets whether the list title should be escaped before rendering.
	 *
	 * @param escapeTitle {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentListRepeater escapeTitle(boolean escapeTitle) {
		return escapeTitle(escapeTitle ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the list title should be escaped before rendering.
	 *
	 * @param escapeTitle {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	public FluentListRepeater escapeTitle(Boolean escapeTitle) {
		list.setEscapeTitle(escapeTitle);
		return this;
	}

	/**
	 * Sets the query name backing the repeater.
	 *
	 * @param queryName
	 *            the query identifier
	 * @return this builder
	 */
	public FluentListRepeater queryName(String queryName) {
		list.setQueryName(queryName);
		return this;
	}

	/**
	 * Sets the model name backing the repeater.
	 *
	 * @param modelName
	 *            the model identifier
	 * @return this builder
	 */
	public FluentListRepeater modelName(String modelName) {
		list.setModelName(modelName);
		return this;
	}

	/**
	 * Sets the condition name evaluated after refresh.
	 *
	 * @param postRefreshConditionName
	 *            the post-refresh condition name
	 * @return this builder
	 */
	public FluentListRepeater postRefreshConditionName(String postRefreshConditionName) {
		list.setPostRefreshConditionName(postRefreshConditionName);
		return this;
	}

	/**
	 * Appends a filter parameter to the repeater query.
	 *
	 * @param filterParameter
	 *            the filter parameter to append
	 * @return this builder
	 */
	public FluentListRepeater addFilterParameter(FluentFilterParameter filterParameter) {
		list.getFilterParameters().add(filterParameter.get());
		return this;
	}

	/**
	 * Appends a query parameter to the repeater query.
	 *
	 * @param parameter
	 *            the parameter to append
	 * @return this builder
	 */
	public FluentListRepeater addParameter(FluentParameter parameter) {
		list.getParameters().add(parameter.get());
		return this;
	}

	/**
	 * Sets the condition name controlling repeater visibility.
	 *
	 * @param invisibleConditionName
	 *            the visibility condition name
	 * @return this builder
	 */
	public FluentListRepeater invisibleConditionName(String invisibleConditionName) {
		list.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	/**
	 * Sets the fixed pixel height for the repeater.
	 *
	 * @param height
	 *            the height in pixels
	 * @return this builder
	 */
	@Override
	public FluentListRepeater pixelHeight(int height) {
		list.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Sets the fixed pixel width for the repeater.
	 *
	 * @param width
	 *            the width in pixels
	 * @return this builder
	 */
	@Override
	public FluentListRepeater pixelWidth(int width) {
		list.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the minimum pixel width for the repeater.
	 *
	 * @param minPixelWidth
	 *            the minimum width in pixels
	 * @return this builder
	 */
	@Override
	public FluentListRepeater minPixelWidth(int minPixelWidth) {
		list.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel width for the repeater.
	 *
	 * @param maxPixelWidth
	 *            the maximum width in pixels
	 * @return this builder
	 */
	@Override
	public FluentListRepeater maxPixelWidth(int maxPixelWidth) {
		list.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel height for the repeater.
	 *
	 * @param maxPixelHeight
	 *            the maximum height in pixels
	 * @return this builder
	 */
	@Override
	public FluentListRepeater maxPixelHeight(int maxPixelHeight) {
		list.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	/**
	 * Sets the minimum pixel height for the repeater.
	 *
	 * @param minPixelHeight
	 *            the minimum height in pixels
	 * @return this builder
	 */
	@Override
	public FluentListRepeater minPixelHeight(int minPixelHeight) {
		list.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the percentage width for the repeater.
	 *
	 * @param percentageWidth
	 *            the width percentage
	 * @return this builder
	 */
	@Override
	public FluentListRepeater percentageWidth(int percentageWidth) {
		list.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	/**
	 * Sets the responsive width for the repeater.
	 *
	 * @param responsiveWidth
	 *            the responsive grid width
	 * @return this builder
	 */
	@Override
	public FluentListRepeater responsiveWidth(int responsiveWidth) {
		list.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	/**
	 * Sets the small-viewport width for the repeater.
	 *
	 * @param sm
	 *            the small-viewport grid width
	 * @return this builder
	 */
	@Override
	public FluentListRepeater sm(int sm) {
		list.setSm(Integer.valueOf(sm));
		return this;
	}

	/**
	 * Sets the medium-viewport width for the repeater.
	 *
	 * @param md
	 *            the medium-viewport grid width
	 * @return this builder
	 */
	@Override
	public FluentListRepeater md(int md) {
		list.setMd(Integer.valueOf(md));
		return this;
	}

	/**
	 * Sets the large-viewport width for the repeater.
	 *
	 * @param lg
	 *            the large-viewport grid width
	 * @return this builder
	 */
	@Override
	public FluentListRepeater lg(int lg) {
		list.setLg(Integer.valueOf(lg));
		return this;
	}

	/**
	 * Sets the extra-large-viewport width for the repeater.
	 *
	 * @param xl
	 *            the extra-large-viewport grid width
	 * @return this builder
	 */
	@Override
	public FluentListRepeater xl(int xl) {
		list.setXl(Integer.valueOf(xl));
		return this;
	}

	/**
	 * Sets the percentage height for the repeater.
	 *
	 * @param percentageHeight
	 *            the height percentage
	 * @return this builder
	 */
	@Override
	public FluentListRepeater percentageHeight(int percentageHeight) {
		list.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped list-repeater metadata
	 */
	@Override
	public ListRepeater get() {
		return list;
	}
}
