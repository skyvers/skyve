package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Colour;

import jakarta.annotation.Nonnull;

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
	public FluentColour(@Nonnull Colour colour) {
		this.colour = colour;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentColour from(@SuppressWarnings("hiding") @Nonnull Colour colour) {
		super.from(colour);
		return this;
	}
	
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public @Nonnull Colour get() {
		return colour;
	}
}
