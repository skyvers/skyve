package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryInputType;

public class FluentGeometry extends FluentChangeableInputWidget<FluentGeometry> implements FluentAbsoluteWidth<FluentGeometry> {
	private Geometry geometry = null;

	public FluentGeometry() {
		geometry = new Geometry();
	}

	public FluentGeometry(Geometry geometry) {
		this.geometry = geometry;
	}

	public FluentGeometry from(@SuppressWarnings("hiding") Geometry geometry) {

		type(geometry.getType());

		absoluteWidth(geometry, this);

		super.from(geometry);
		return this;
	}

	public FluentGeometry type(GeometryInputType type) {
		geometry.setType(type);
		return this;
	}

	@Override
	public FluentGeometry pixelWidth(int width) {
		geometry.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public Geometry get() {
		return geometry;
	}
}
