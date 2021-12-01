package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.StaticImage;

public class FluentStaticImage extends FluentWidget implements FluentRelativeSize<FluentStaticImage> {
	private StaticImage image = null;
	
	public FluentStaticImage() {
		this.image = new StaticImage();
	}
	
	public FluentStaticImage(StaticImage image) {
		this.image = image;
	}
	
	public FluentStaticImage from(@SuppressWarnings("hiding") StaticImage image) {
		relativeFile(image.getRelativeFile());
		relativeSize(image, this);
		invisibleConditionName(image.getInvisibleConditionName());
		
		return this;
	}
	
	public FluentStaticImage relativeFile(String relativeFile) {
		image.setRelativeFile(relativeFile);
		return this;
	}
	
	@Override
	public FluentStaticImage pixelWidth(int pixelWidth) {
		image.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	@Override
	public FluentStaticImage responsiveWidth(int responsiveWidth) {
		image.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	@Override
	public FluentStaticImage sm(int sm) {
		image.setSm(Integer.valueOf(sm));
		return this;
	}
	
	@Override
	public FluentStaticImage md(int md) {
		image.setMd(Integer.valueOf(md));
		return this;
	}
	
	@Override
	public FluentStaticImage lg(int lg) {
		image.setLg(Integer.valueOf(lg));
		return this;
	}

	@Override
	public FluentStaticImage xl(int xl) {
		image.setXl(Integer.valueOf(xl));
		return this;
	}

	@Override
	public FluentStaticImage percentageWidth(int percentageWidth) {
		image.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	@Override
	public FluentStaticImage minPixelWidth(int minPixelWidth) {
		image.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	@Override
	public FluentStaticImage maxPixelWidth(int maxPixelWidth) {
		image.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	@Override
	public FluentStaticImage pixelHeight(int pixelHeight) {
		image.setPixelHeight(Integer.valueOf(pixelHeight));
		return this;
	}

	@Override
	public FluentStaticImage percentageHeight(int percentageHeight) {
		image.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	@Override
	public FluentStaticImage minPixelHeight(int minPixelHeight) {
		image.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentStaticImage maxPixelHeight(int maxPixelHeight) {
		image.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}
	
	public FluentStaticImage invisibleConditionName(String invisibleConditionName) {
		image.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	@Override
	public StaticImage get() {
		return image;
	}
}
