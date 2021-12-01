package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Image;

public class FluentImage extends FluentConstrainableField<FluentImage> {
	private Image image = null;
	
	public FluentImage() {
		image = new Image();
	}

	public FluentImage(Image image) {
		this.image = image;
	}

	public FluentImage from(@SuppressWarnings("hiding") Image image) {
		super.from(image);
		return this;
	}
	
	@Override
	public Image get() {
		return image;
	}
}
