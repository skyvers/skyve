package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryInputType;

/**
 * Builds {@link Geometry} widget metadata using a fluent API.
 */
public class FluentGeometry extends FluentChangeableInputWidget<FluentGeometry> implements FluentAbsoluteWidth<FluentGeometry> {
	private Geometry geometry = null;

	/**
	 * Creates a fluent builder backed by a new {@link Geometry} metadata instance.
	 */
	public FluentGeometry() {
		geometry = new Geometry();
	}

	/**
	 * Creates a fluent builder backed by the supplied {@link Geometry} metadata instance.
	 *
	 * @param geometry the metadata instance to mutate
	 */
	public FluentGeometry(Geometry geometry) {
		this.geometry = geometry;
	}

	/**
	 * Copies geometry metadata into this fluent builder.
	 *
	 * @param geometry
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentGeometry from(@SuppressWarnings("hiding") Geometry geometry) {

		type(geometry.getType());

		absoluteWidth(geometry, this);

		super.from(geometry);
		return this;
	}

	/**
	 * Sets the input type for this geometry widget.
	 *
	 * @param type the geometry input type
	 * @return this builder
	 */
	public FluentGeometry type(GeometryInputType type) {
		geometry.setType(type);
		return this;
	}

	/**
	 * Sets the pixel width of this geometry widget.
	 *
	 * @param width the pixel width
	 * @return this builder
	 */
	@Override
	public FluentGeometry pixelWidth(int width) {
		geometry.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Returns the wrapped {@link Geometry} metadata instance.
	 *
	 * @return the mutable geometry metadata being configured
	 */
	@Override
	public Geometry get() {
		return geometry;
	}
}
