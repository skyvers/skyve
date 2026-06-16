package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Image;

/**
 * Provides a fluent builder for FluentImage metadata.
 */
public class FluentImage extends FluentConstrainableField<FluentImage> {
	private Image image = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentImage() {
		image = new Image();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentImage(Image image) {
		this.image = image;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentImage from(@SuppressWarnings("hiding") Image image) {
		super.from(image);
		return this;
	}
	
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public Image get() {
		return image;
	}
}
