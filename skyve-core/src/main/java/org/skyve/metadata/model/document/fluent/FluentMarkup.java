package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Markup;

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
	public FluentMarkup(Markup markup) {
		this.markup = markup;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentMarkup from(@SuppressWarnings("hiding") Markup markup) {
		super.from(markup);
		return this;
	}
	
	@Override
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public Markup get() {
		return markup;
	}
}
