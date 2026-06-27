package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Geometry;

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
	public FluentGeometry(Geometry geometry) {
		this.geometry = geometry;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentGeometry from(@SuppressWarnings("hiding") Geometry geometry) {
		super.from(geometry);
		return this;
	}
	
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public Geometry get() {
		return geometry;
	}
}
