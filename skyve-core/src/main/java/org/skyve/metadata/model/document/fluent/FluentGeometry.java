package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Geometry;

import jakarta.annotation.Nonnull;

/**
 * Provides a fluent builder for FluentGeometry metadata.
 */
public class FluentGeometry extends FluentField<FluentGeometry> {
	private Geometry geometry = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentGeometry() {
		geometry = new Geometry();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentGeometry(@Nonnull Geometry geometry) {
		this.geometry = geometry;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentGeometry from(@SuppressWarnings("hiding") @Nonnull Geometry geometry) {
		super.from(geometry);
		return this;
	}
	
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public @Nonnull Geometry get() {
		return geometry;
	}
}
