package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.LoadingType;
import org.skyve.impl.metadata.view.widget.MapDisplay;

/**
 * Builds {@link MapDisplay} widget metadata using a fluent API.
 */
public class FluentMapDisplay extends FluentWidget implements FluentRelativeSize<FluentMapDisplay> {
	private MapDisplay map = null;

	/**
	 * Creates a fluent builder backed by a new {@link MapDisplay} metadata instance.
	 */
	public FluentMapDisplay() {
		map = new MapDisplay();
	}

	/**
	 * Creates a fluent builder backed by the supplied {@link MapDisplay} metadata instance.
	 *
	 * @param map the metadata instance to mutate
	 */
	public FluentMapDisplay(MapDisplay map) {
		this.map = map;
	}

	/**
	 * Copies map-display metadata into this fluent builder.
	 *
	 * @param map
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentMapDisplay from(@SuppressWarnings("hiding") MapDisplay map) {

		modelName(map.getModelName());
		loading(map.getLoading());
		Integer i = map.getRefreshTimeInSeconds();
		if (i != null) {
			refreshTimeInSeconds(i.intValue());
		}
		Boolean b = map.getShowRefreshControls();
		if (b != null) {
			showRefreshControls(b.booleanValue());
		}
		invisibleConditionName(map.getInvisibleConditionName());

		relativeSize(map, this);

		return this;
	}

	/**
	 * Sets the model name for this map display widget.
	 *
	 * @param modelName the model name
	 * @return this builder
	 */
	public FluentMapDisplay modelName(String modelName) {
		map.setModelName(modelName);
		return this;
	}

	/**
	 * Sets the loading type for this map display widget.
	 *
	 * @param loading the loading type
	 * @return this builder
	 */
	public FluentMapDisplay loading(LoadingType loading) {
		map.setLoading(loading);
		return this;
	}

	/**
	 * Sets the auto-refresh interval in seconds.
	 *
	 * @param refreshTimeInSeconds the refresh interval
	 * @return this builder
	 */
	public FluentMapDisplay refreshTimeInSeconds(int refreshTimeInSeconds) {
		map.setRefreshTimeInSeconds(Integer.valueOf(refreshTimeInSeconds));
		return this;
	}

	/**
	 * Sets whether the refresh controls are shown on this map display.
	 *
	 * @param showRefreshControls {@code true} to show refresh controls
	 * @return this builder
	 */
	public FluentMapDisplay showRefreshControls(boolean showRefreshControls) {
		map.setShowRefreshControls(showRefreshControls ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the condition name that hides this map display widget.
	 *
	 * @param invisibleConditionName the condition name
	 * @return this builder
	 */
	public FluentMapDisplay invisibleConditionName(String invisibleConditionName) {
		map.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	/**
	 * Sets the pixel height of this map display widget.
	 *
	 * @param height the pixel height
	 * @return this builder
	 */
	@Override
	public FluentMapDisplay pixelHeight(int height) {
		map.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Sets the pixel width of this map display widget.
	 *
	 * @param width the pixel width
	 * @return this builder
	 */
	@Override
	public FluentMapDisplay pixelWidth(int width) {
		map.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the minimum pixel width of this map display widget.
	 *
	 * @param minPixelWidth the minimum pixel width
	 * @return this builder
	 */
	@Override
	public FluentMapDisplay minPixelWidth(int minPixelWidth) {
		map.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel width of this map display widget.
	 *
	 * @param maxPixelWidth the maximum pixel width
	 * @return this builder
	 */
	@Override
	public FluentMapDisplay maxPixelWidth(int maxPixelWidth) {
		map.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel height of this map display widget.
	 *
	 * @param maxPixelHeight the maximum pixel height
	 * @return this builder
	 */
	@Override
	public FluentMapDisplay maxPixelHeight(int maxPixelHeight) {
		map.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	/**
	 * Sets the minimum pixel height of this map display widget.
	 *
	 * @param minPixelHeight the minimum pixel height
	 * @return this builder
	 */
	@Override
	public FluentMapDisplay minPixelHeight(int minPixelHeight) {
		map.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the percentage width of this map display widget.
	 *
	 * @param percentageWidth the percentage width
	 * @return this builder
	 */
	@Override
	public FluentMapDisplay percentageWidth(int percentageWidth) {
		map.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	/**
	 * Sets the responsive width breakpoint for this map display widget.
	 *
	 * @param responsiveWidth the responsive width
	 * @return this builder
	 */
	@Override
	public FluentMapDisplay responsiveWidth(int responsiveWidth) {
		map.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	/**
	 * Sets the small breakpoint column span for this map display widget.
	 *
	 * @param sm the small breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentMapDisplay sm(int sm) {
		map.setSm(Integer.valueOf(sm));
		return this;
	}

	/**
	 * Sets the medium breakpoint column span for this map display widget.
	 *
	 * @param md the medium breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentMapDisplay md(int md) {
		map.setMd(Integer.valueOf(md));
		return this;
	}

	/**
	 * Sets the large breakpoint column span for this map display widget.
	 *
	 * @param lg the large breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentMapDisplay lg(int lg) {
		map.setLg(Integer.valueOf(lg));
		return this;
	}

	/**
	 * Sets the extra-large breakpoint column span for this map display widget.
	 *
	 * @param xl the extra-large breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentMapDisplay xl(int xl) {
		map.setXl(Integer.valueOf(xl));
		return this;
	}

	/**
	 * Sets the percentage height of this map display widget.
	 *
	 * @param percentageHeight the percentage height
	 * @return this builder
	 */
	@Override
	public FluentMapDisplay percentageHeight(int percentageHeight) {
		map.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	/**
	 * Returns the wrapped {@link MapDisplay} metadata instance.
	 *
	 * @return the mutable map-display metadata being configured
	 */
	@Override
	public MapDisplay get() {
		return map;
	}
}
