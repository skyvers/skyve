package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.GeometryInputType;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;

/**
 * Builds {@link GeometryMap} widget metadata using a fluent API.
 */
public class FluentGeometryMap extends FluentInputWidget<FluentGeometryMap> implements FluentRelativeSize<FluentGeometryMap> {
	private GeometryMap map = null;

	/**
	 * Creates a fluent builder backed by a new {@link GeometryMap} metadata instance.
	 */
	public FluentGeometryMap() {
		map = new GeometryMap();
	}

	/**
	 * Creates a fluent builder backed by the supplied {@link GeometryMap} metadata instance.
	 *
	 * @param map the metadata instance to mutate
	 */
	public FluentGeometryMap(GeometryMap map) {
		this.map = map;
	}

	/**
	 * Copies geometry-map metadata into this fluent builder.
	 *
	 * @param map
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentGeometryMap from(@SuppressWarnings("hiding") GeometryMap map) {

		type(map.getType());

		relativeSize(map, this);

		map.getChangedActions().forEach(c -> addAction(FluentEventAction.from(c)));

		super.from(map);
		return this;
	}

	/**
	 * Sets the input type for this geometry map widget.
	 *
	 * @param type the geometry input type
	 * @return this builder
	 */
	public FluentGeometryMap type(GeometryInputType type) {
		map.setType(type);
		return this;
	}

	/**
	 * Sets the pixel height of this geometry map widget.
	 *
	 * @param height the pixel height
	 * @return this builder
	 */
	@Override
	public FluentGeometryMap pixelHeight(int height) {
		map.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Sets the pixel width of this geometry map widget.
	 *
	 * @param width the pixel width
	 * @return this builder
	 */
	@Override
	public FluentGeometryMap pixelWidth(int width) {
		map.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the minimum pixel width of this geometry map widget.
	 *
	 * @param minPixelWidth the minimum pixel width
	 * @return this builder
	 */
	@Override
	public FluentGeometryMap minPixelWidth(int minPixelWidth) {
		map.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel width of this geometry map widget.
	 *
	 * @param maxPixelWidth the maximum pixel width
	 * @return this builder
	 */
	@Override
	public FluentGeometryMap maxPixelWidth(int maxPixelWidth) {
		map.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel height of this geometry map widget.
	 *
	 * @param maxPixelHeight the maximum pixel height
	 * @return this builder
	 */
	@Override
	public FluentGeometryMap maxPixelHeight(int maxPixelHeight) {
		map.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	/**
	 * Sets the minimum pixel height of this geometry map widget.
	 *
	 * @param minPixelHeight the minimum pixel height
	 * @return this builder
	 */
	@Override
	public FluentGeometryMap minPixelHeight(int minPixelHeight) {
		map.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the percentage width of this geometry map widget.
	 *
	 * @param percentageWidth the percentage width
	 * @return this builder
	 */
	@Override
	public FluentGeometryMap percentageWidth(int percentageWidth) {
		map.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	/**
	 * Sets the percentage height of this geometry map widget.
	 *
	 * @param percentageHeight the percentage height
	 * @return this builder
	 */
	@Override
	public FluentGeometryMap percentageHeight(int percentageHeight) {
		map.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	/**
	 * Sets the responsive width breakpoint for this geometry map widget.
	 *
	 * @param responsiveWidth the responsive width
	 * @return this builder
	 */
	@Override
	public FluentGeometryMap responsiveWidth(int responsiveWidth) {
		map.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	/**
	 * Sets the small breakpoint column span for this geometry map widget.
	 *
	 * @param sm the small breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentGeometryMap sm(int sm) {
		map.setSm(Integer.valueOf(sm));
		return this;
	}

	/**
	 * Sets the medium breakpoint column span for this geometry map widget.
	 *
	 * @param md the medium breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentGeometryMap md(int md) {
		map.setMd(Integer.valueOf(md));
		return this;
	}

	/**
	 * Sets the large breakpoint column span for this geometry map widget.
	 *
	 * @param lg the large breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentGeometryMap lg(int lg) {
		map.setLg(Integer.valueOf(lg));
		return this;
	}

	/**
	 * Sets the extra-large breakpoint column span for this geometry map widget.
	 *
	 * @param xl the extra-large breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentGeometryMap xl(int xl) {
		map.setXl(Integer.valueOf(xl));
		return this;
	}

	/**
	 * Appends a changed-action definition to the wrapped map metadata.
	 *
	 * @param action
	 *            the changed-action definition to append
	 * @return this builder
	 */
	public FluentGeometryMap addAction(FluentEventAction action) {
		map.getChangedActions().add(action.get());
		return this;
	}

	/**
	 * Returns the wrapped {@link GeometryMap} metadata instance.
	 *
	 * @return the mutable geometry-map metadata being configured
	 */
	@Override
	public GeometryMap get() {
		return map;
	}
}
