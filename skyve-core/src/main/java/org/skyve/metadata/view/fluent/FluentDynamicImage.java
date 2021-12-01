package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.DynamicImage;

public class FluentDynamicImage extends FluentWidget {
	private DynamicImage image = null;
	
	public FluentDynamicImage() {
		image = new DynamicImage();
	}

	public FluentDynamicImage(DynamicImage image) {
		this.image = image;
	}

	public FluentDynamicImage from(@SuppressWarnings("hiding") DynamicImage image) {
		return this;
	}

	@Override
	public DynamicImage get() {
		return image;
	}
}
