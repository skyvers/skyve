package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.LoadingType;
import org.skyve.impl.metadata.view.widget.MapDisplay;

public class FluentMapDisplay extends FluentWidget implements FluentRelativeSize<FluentMapDisplay> {
	private MapDisplay map = null;

	public FluentMapDisplay() {
		map = new MapDisplay();
	}

	public FluentMapDisplay(MapDisplay map) {
		this.map = map;
	}

	public FluentMapDisplay from(@SuppressWarnings("hiding") MapDisplay map) {

		modelName(map.getModelName());
		loading(map.getLoading());
		refreshTimeInSeconds(map.getRefreshTimeInSeconds());
		showRefreshControls(map.getShowRefreshControls());
		invisibleConditionName(map.getInvisibleConditionName());

		relativeSize(map, this);

		return this;
	}

	public FluentMapDisplay modelName(String modelName) {
		map.setModelName(modelName);
		return this;
	}

	public FluentMapDisplay loading(LoadingType loading) {
		map.setLoading(loading);
		return this;
	}

	public FluentMapDisplay refreshTimeInSeconds(int refreshTimeInSeconds) {
		map.setRefreshTimeInSeconds(Integer.valueOf(refreshTimeInSeconds));
		return this;
	}

	public FluentMapDisplay showRefreshControls(boolean showRefreshControls) {
		map.setShowRefreshControls(showRefreshControls ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentMapDisplay invisibleConditionName(String invisibleConditionName) {
		map.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	@Override
	public FluentMapDisplay pixelHeight(int height) {
		map.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public FluentMapDisplay pixelWidth(int width) {
		map.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentMapDisplay minPixelWidth(int minPixelWidth) {
		map.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	@Override
	public FluentMapDisplay maxPixelWidth(int maxPixelWidth) {
		map.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	@Override
	public FluentMapDisplay maxPixelHeight(int maxPixelHeight) {
		map.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	@Override
	public FluentMapDisplay minPixelHeight(int minPixelHeight) {
		map.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentMapDisplay percentageWidth(int percentageWidth) {
		map.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	@Override
	public FluentMapDisplay responsiveWidth(int responsiveWidth) {
		map.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	@Override
	public FluentMapDisplay sm(int sm) {
		map.setSm(Integer.valueOf(sm));
		return this;
	}

	@Override
	public FluentMapDisplay md(int md) {
		map.setMd(Integer.valueOf(md));
		return this;
	}

	@Override
	public FluentMapDisplay lg(int lg) {
		map.setLg(Integer.valueOf(lg));
		return this;
	}

	@Override
	public FluentMapDisplay xl(int xl) {
		map.setXl(Integer.valueOf(xl));
		return this;
	}

	@Override
	public FluentMapDisplay percentageHeight(int percentageHeight) {
		map.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	@Override
	public MapDisplay get() {
		return map;
	}
}
