package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Markup;

import jakarta.annotation.Nonnull;

/**
 * Provides a fluent builder for FluentMarkup metadata.
 */
public class FluentMarkup extends FluentConstrainableField<FluentMarkup> {
	private Markup markup = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentMarkup() {
		markup = new Markup();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentMarkup(@Nonnull Markup markup) {
		this.markup = markup;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentMarkup from(@SuppressWarnings("hiding") @Nonnull Markup markup) {
		super.from(markup);
		return this;
	}
	
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public @Nonnull Markup get() {
		return markup;
	}
}
