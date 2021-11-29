package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Image;

public class FluentImage extends FluentConstrainableField<FluentImage> {
	private Image image = new Image();
	
	public FluentImage() {
		// nothing to see
	}

	public FluentImage(Image image) {
		super(image);
	}
	
	@Override
	public Image get() {
		return image;
	}
}
