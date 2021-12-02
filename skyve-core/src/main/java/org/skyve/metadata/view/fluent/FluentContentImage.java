package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;

public class FluentContentImage extends FluentInputWidget<FluentContentImage> {
	private ContentImage image = null;
	
	public FluentContentImage() {
		image = new ContentImage();
	}

	public FluentContentImage(ContentImage image) {
		this.image = image;
	}

	public FluentContentImage from(@SuppressWarnings("hiding") ContentImage image) {
		super.from(image);
		return this;
	}

	@Override
	public ContentImage get() {
		return image;
	}
}
