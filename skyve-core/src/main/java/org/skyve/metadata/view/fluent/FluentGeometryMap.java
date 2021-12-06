package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.GeometryInputType;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;

public class FluentGeometryMap extends FluentInputWidget<FluentGeometryMap> implements FluentRelativeSize<FluentGeometryMap> {
	private GeometryMap map = null;

	public FluentGeometryMap() {
		map = new GeometryMap();
	}

	public FluentGeometryMap(GeometryMap map) {
		this.map = map;
	}

	public FluentGeometryMap from(@SuppressWarnings("hiding") GeometryMap map) {

		type(map.getType());

		relativeSize(map, this);

		map.getChangedActions().forEach(c -> addAction(FluentEventAction.from(c)));

		super.from(map);
		return this;
	}

	public FluentGeometryMap type(GeometryInputType type) {
		map.setType(type);
		return this;
	}

	@Override
	public FluentGeometryMap pixelHeight(int height) {
		map.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public FluentGeometryMap pixelWidth(int width) {
		map.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentGeometryMap minPixelWidth(int minPixelWidth) {
		map.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	@Override
	public FluentGeometryMap maxPixelWidth(int maxPixelWidth) {
		map.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	@Override
	public FluentGeometryMap maxPixelHeight(int maxPixelHeight) {
		map.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	@Override
	public FluentGeometryMap minPixelHeight(int minPixelHeight) {
		map.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentGeometryMap percentageWidth(int percentageWidth) {
		map.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	@Override
	public FluentGeometryMap percentageHeight(int percentageHeight) {
		map.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	@Override
	public FluentGeometryMap responsiveWidth(int responsiveWidth) {
		map.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	@Override
	public FluentGeometryMap sm(int sm) {
		map.setSm(Integer.valueOf(sm));
		return this;
	}

	@Override
	public FluentGeometryMap md(int md) {
		map.setMd(Integer.valueOf(md));
		return this;
	}

	@Override
	public FluentGeometryMap lg(int lg) {
		map.setLg(Integer.valueOf(lg));
		return this;
	}

	@Override
	public FluentGeometryMap xl(int xl) {
		map.setXl(Integer.valueOf(xl));
		return this;
	}

	public FluentGeometryMap addAction(FluentEventAction action) {
		map.getChangedActions().add(action.get());
		return this;
	}

	@Override
	public GeometryMap get() {
		return map;
	}
}
