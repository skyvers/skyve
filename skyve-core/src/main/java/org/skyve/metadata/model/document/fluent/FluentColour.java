package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Colour;

/**
 * Provides a fluent builder for FluentColour metadata.
 */
public class FluentColour extends FluentConvertibleField<FluentColour> {
	private Colour colour = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentColour() {
		colour = new Colour();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentColour(Colour colour) {
		this.colour = colour;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentColour from(@SuppressWarnings("hiding") Colour colour) {
		super.from(colour);
		return this;
	}
	
	@Override
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public Colour get() {
		return colour;
	}
}
