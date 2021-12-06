package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.DynamicImage;

public class FluentDynamicImage extends FluentWidget implements FluentRelativeSize<FluentDynamicImage> {
	private DynamicImage image = null;

	public FluentDynamicImage() {
		image = new DynamicImage();
	}

	public FluentDynamicImage(DynamicImage image) {
		this.image = image;
	}

	public FluentDynamicImage from(@SuppressWarnings("hiding") DynamicImage image) {
		name(image.getName());
		Integer i = image.getImageInitialPixelWidth();
		if (i != null) {
			imageInitialPixelWidth(i.intValue());
		}
		i = image.getImageInitialPixelHeight();
		if (i != null) {
			imageInitialPixelHeight(i.intValue());
		}

		relativeSize(image, this);

		image.getParameters().forEach(p -> addParameter(new FluentParameter().from(p)));

		return this;
	}

	public FluentDynamicImage name(String name) {
		image.setName(name);
		return this;
	}

	public FluentDynamicImage imageInitialPixelWidth(int imageInitialPixelWidth) {
		image.setImageInitialPixelWidth(Integer.valueOf(imageInitialPixelWidth));
		return this;
	}

	public FluentDynamicImage imageInitialPixelHeight(int imageInitialPixelHeight) {
		image.setImageInitialPixelHeight(Integer.valueOf(imageInitialPixelHeight));
		return this;
	}

	@Override
	public FluentDynamicImage pixelHeight(int height) {
		image.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public FluentDynamicImage pixelWidth(int width) {
		image.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentDynamicImage minPixelWidth(int minPixelWidth) {
		image.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	@Override
	public FluentDynamicImage maxPixelWidth(int maxPixelWidth) {
		image.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	@Override
	public FluentDynamicImage maxPixelHeight(int maxPixelHeight) {
		image.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	@Override
	public FluentDynamicImage minPixelHeight(int minPixelHeight) {
		image.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentDynamicImage percentageWidth(int percentageWidth) {
		image.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	@Override
	public FluentDynamicImage percentageHeight(int percentageHeight) {
		image.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	@Override
	public FluentDynamicImage responsiveWidth(int responsiveWidth) {
		image.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	@Override
	public FluentDynamicImage sm(int sm) {
		image.setSm(Integer.valueOf(sm));
		return this;
	}

	@Override
	public FluentDynamicImage md(int md) {
		image.setMd(Integer.valueOf(md));
		return this;
	}

	@Override
	public FluentDynamicImage lg(int lg) {
		image.setLg(Integer.valueOf(lg));
		return this;
	}

	@Override
	public FluentDynamicImage xl(int xl) {
		image.setXl(Integer.valueOf(xl));
		return this;
	}

	public FluentDynamicImage addParameter(FluentParameter parameter) {
		image.getParameters().add(parameter.get());
		return this;
	}

	@Override
	public DynamicImage get() {
		return image;
	}
}
