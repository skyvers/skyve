package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;

public class FluentContentImage extends FluentWidget {
	private ContentImage image = null;
	
	public FluentContentImage() {
		image = new ContentImage();
	}

	public FluentContentImage(ContentImage image) {
		this.image = image;
	}

	public FluentContentImage from(@SuppressWarnings("hiding") ContentImage image) {
		return this;
	}

	@Override
	public ContentImage get() {
		return image;
	}
}
