package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Content;

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
	public FluentContent(Content content) {
		this.content = content;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentContent from(@SuppressWarnings("hiding") Content content) {
		super.from(content);
		return this;
	}
	
	@Override
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public Content get() {
		return content;
	}
}
