package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryInputType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds {@link Geometry} widget metadata using a fluent API.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
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
	public FluentGeometry(@Nonnull Geometry geometry) {
		this.geometry = geometry;
	}

	/**
	 * Copies geometry metadata into this fluent builder.
	 *
	 * @param geometry
	 *            the source metadata to copy
	 * @return this builder
	 */
	public @Nonnull FluentGeometry from(@SuppressWarnings("hiding") @Nonnull Geometry geometry) {
		type(geometry.getType());
		absoluteWidth(geometry, this);
		super.from(geometry);
		geometry.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return this;
	}

	/**
	 * Sets the input type for this geometry widget.
	 *
	 * @param type the geometry input type
	 * @return this builder
	 */
	public @Nonnull FluentGeometry type(@Nullable GeometryInputType type) {
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
	public @Nonnull FluentGeometry pixelWidth(int width) {
		geometry.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentGeometry putProperty(@Nonnull String k, @Nonnull String v) {
		geometry.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the wrapped {@link Geometry} metadata instance.
	 *
	 * @return the mutable geometry metadata being configured
	 */
	@Override
	public @Nonnull Geometry get() {
		return geometry;
	}
}
