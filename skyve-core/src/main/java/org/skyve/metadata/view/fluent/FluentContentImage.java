package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;

public class FluentContentImage extends FluentInputWidget<FluentContentImage> implements FluentRelativeSize<FluentContentImage> {
	private ContentImage image = null;

	public FluentContentImage() {
		image = new ContentImage();
	}

	public FluentContentImage(ContentImage image) {
		this.image = image;
	}

	public FluentContentImage from(@SuppressWarnings("hiding") ContentImage image) {

		editable(image.getEditable());

		relativeSize(image, this);

		super.from(image);
		return this;
	}

	public FluentContentImage editable(Boolean editable) {
		image.setEditable(editable);
		return this;
	}

	@Override
	public FluentContentImage pixelHeight(int height) {
		image.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public FluentContentImage pixelWidth(int width) {
		image.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentContentImage minPixelWidth(int minPixelWidth) {
		image.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	@Override
	public FluentContentImage maxPixelWidth(int maxPixelWidth) {
		image.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	@Override
	public FluentContentImage maxPixelHeight(int maxPixelHeight) {
		image.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	@Override
	public FluentContentImage minPixelHeight(int minPixelHeight) {
		image.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentContentImage percentageWidth(int percentageWidth) {
		image.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	@Override
	public FluentContentImage percentageHeight(int percentageHeight) {
		image.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	@Override
	public FluentContentImage responsiveWidth(int responsiveWidth) {
		image.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	@Override
	public FluentContentImage sm(int sm) {
		image.setSm(Integer.valueOf(sm));
		return this;
	}

	@Override
	public FluentContentImage md(int md) {
		image.setMd(Integer.valueOf(md));
		return this;
	}

	@Override
	public FluentContentImage lg(int lg) {
		image.setLg(Integer.valueOf(lg));
		return this;
	}

	@Override
	public FluentContentImage xl(int xl) {
		image.setXl(Integer.valueOf(xl));
		return this;
	}

	@Override
	public ContentImage get() {
		return image;
	}
}
