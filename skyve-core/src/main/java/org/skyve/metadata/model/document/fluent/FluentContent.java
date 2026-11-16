package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Content;

import jakarta.annotation.Nonnull;

/**
 * Provides a fluent builder for FluentContent metadata.
 */
public class FluentContent extends FluentConstrainableField<FluentContent> {
	private Content content = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentContent() {
		content = new Content();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentContent(@Nonnull Content content) {
		this.content = content;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentContent from(@SuppressWarnings("hiding") @Nonnull Content content) {
		super.from(content);
		return this;
	}
	
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public @Nonnull Content get() {
		return content;
	}
}
