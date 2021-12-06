package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;

public class FluentListRepeater extends FluentWidget implements FluentRelativeSize<FluentListRepeater> {
	private ListRepeater list = null;

	public FluentListRepeater() {
		list = new ListRepeater();
	}

	public FluentListRepeater(ListRepeater list) {
		this.list = list;
	}

	public FluentListRepeater from(@SuppressWarnings("hiding") ListRepeater list) {

		showColumnHeaders(list.getShowColumnHeaders());
		showGrid(list.getShowGrid());

		title(list.getTitle());
		queryName(list.getQueryName());
		modelName(list.getModelName());
		postRefreshConditionName(list.getPostRefreshConditionName());
		invisibleConditionName(list.getInvisibleConditionName());

		relativeSize(list, this);

		list.getParameters().forEach(p -> addParameter(new FluentParameter().from(p)));

		list.getFilterParameters().forEach(f -> addFilterParameter(new FluentFilterParameter().from(f)));

		return this;
	}

	public FluentListRepeater showColumnHeaders(boolean showColumnHeaders) {
		list.setShowColumnHeaders(showColumnHeaders ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListRepeater showGrid(boolean showGrid) {
		list.setShowGrid(showGrid ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListRepeater title(String title) {
		list.setTitle(title);
		return this;
	}

	public FluentListRepeater queryName(String queryName) {
		list.setQueryName(queryName);
		return this;
	}

	public FluentListRepeater modelName(String modelName) {
		list.setModelName(modelName);
		return this;
	}

	public FluentListRepeater postRefreshConditionName(String postRefreshConditionName) {
		list.setPostRefreshConditionName(postRefreshConditionName);
		return this;
	}

	public FluentListRepeater addFilterParameter(FluentFilterParameter filterParameter) {
		list.getFilterParameters().add(filterParameter.get());
		return this;
	}

	public FluentListRepeater addParameter(FluentParameter parameter) {
		list.getParameters().add(parameter.get());
		return this;
	}

	public FluentListRepeater invisibleConditionName(String invisibleConditionName) {
		list.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	@Override
	public FluentListRepeater pixelHeight(int height) {
		list.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public FluentListRepeater pixelWidth(int width) {
		list.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentListRepeater minPixelWidth(int minPixelWidth) {
		list.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	@Override
	public FluentListRepeater maxPixelWidth(int maxPixelWidth) {
		list.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	@Override
	public FluentListRepeater maxPixelHeight(int maxPixelHeight) {
		list.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	@Override
	public FluentListRepeater minPixelHeight(int minPixelHeight) {
		list.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentListRepeater percentageWidth(int percentageWidth) {
		list.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	@Override
	public FluentListRepeater responsiveWidth(int responsiveWidth) {
		list.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	@Override
	public FluentListRepeater sm(int sm) {
		list.setSm(Integer.valueOf(sm));
		return this;
	}

	@Override
	public FluentListRepeater md(int md) {
		list.setMd(Integer.valueOf(md));
		return this;
	}

	@Override
	public FluentListRepeater lg(int lg) {
		list.setLg(Integer.valueOf(lg));
		return this;
	}

	@Override
	public FluentListRepeater xl(int xl) {
		list.setXl(Integer.valueOf(xl));
		return this;
	}

	@Override
	public FluentListRepeater percentageHeight(int percentageHeight) {
		list.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	@Override
	public ListRepeater get() {
		return list;
	}

}
