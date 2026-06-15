package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.DynamicImage;

/**
 * Builds {@link DynamicImage} widget metadata using a fluent API.
 */
public class FluentDynamicImage extends FluentWidget implements FluentRelativeSize<FluentDynamicImage> {
	private DynamicImage image = null;

	/**
	 * Creates a fluent builder backed by a new {@link DynamicImage} metadata instance.
	 */
	public FluentDynamicImage() {
		image = new DynamicImage();
	}

	/**
	 * Creates a fluent builder backed by the supplied {@link DynamicImage} metadata instance.
	 *
	 * @param image the metadata instance to mutate
	 */
	public FluentDynamicImage(DynamicImage image) {
		this.image = image;
	}

	/**
	 * Copies dynamic-image metadata into this fluent builder.
	 */
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

	/**
	 * Sets the name of the dynamic image renderer.
	 *
	 * @param name the renderer name
	 * @return this builder
	 */
	public FluentDynamicImage name(String name) {
		image.setName(name);
		return this;
	}

	/**
	 * Sets the initial pixel width passed to the image renderer.
	 *
	 * @param imageInitialPixelWidth the initial pixel width
	 * @return this builder
	 */
	public FluentDynamicImage imageInitialPixelWidth(int imageInitialPixelWidth) {
		image.setImageInitialPixelWidth(Integer.valueOf(imageInitialPixelWidth));
		return this;
	}

	/**
	 * Sets the initial pixel height passed to the image renderer.
	 *
	 * @param imageInitialPixelHeight the initial pixel height
	 * @return this builder
	 */
	public FluentDynamicImage imageInitialPixelHeight(int imageInitialPixelHeight) {
		image.setImageInitialPixelHeight(Integer.valueOf(imageInitialPixelHeight));
		return this;
	}

	/**
	 * Sets the pixel height of this dynamic image widget.
	 *
	 * @param height the pixel height
	 * @return this builder
	 */
	@Override
	public FluentDynamicImage pixelHeight(int height) {
		image.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Sets the pixel width of this dynamic image widget.
	 *
	 * @param width the pixel width
	 * @return this builder
	 */
	@Override
	public FluentDynamicImage pixelWidth(int width) {
		image.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the minimum pixel width of this dynamic image widget.
	 *
	 * @param minPixelWidth the minimum pixel width
	 * @return this builder
	 */
	@Override
	public FluentDynamicImage minPixelWidth(int minPixelWidth) {
		image.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel width of this dynamic image widget.
	 *
	 * @param maxPixelWidth the maximum pixel width
	 * @return this builder
	 */
	@Override
	public FluentDynamicImage maxPixelWidth(int maxPixelWidth) {
		image.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel height of this dynamic image widget.
	 *
	 * @param maxPixelHeight the maximum pixel height
	 * @return this builder
	 */
	@Override
	public FluentDynamicImage maxPixelHeight(int maxPixelHeight) {
		image.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	/**
	 * Sets the minimum pixel height of this dynamic image widget.
	 *
	 * @param minPixelHeight the minimum pixel height
	 * @return this builder
	 */
	@Override
	public FluentDynamicImage minPixelHeight(int minPixelHeight) {
		image.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the percentage width of this dynamic image widget.
	 *
	 * @param percentageWidth the percentage width
	 * @return this builder
	 */
	@Override
	public FluentDynamicImage percentageWidth(int percentageWidth) {
		image.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	/**
	 * Sets the percentage height of this dynamic image widget.
	 *
	 * @param percentageHeight the percentage height
	 * @return this builder
	 */
	@Override
	public FluentDynamicImage percentageHeight(int percentageHeight) {
		image.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	/**
	 * Sets the responsive width breakpoint for this dynamic image widget.
	 *
	 * @param responsiveWidth the responsive width
	 * @return this builder
	 */
	@Override
	public FluentDynamicImage responsiveWidth(int responsiveWidth) {
		image.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	/**
	 * Sets the small breakpoint column span for this dynamic image widget.
	 *
	 * @param sm the small breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentDynamicImage sm(int sm) {
		image.setSm(Integer.valueOf(sm));
		return this;
	}

	/**
	 * Sets the medium breakpoint column span for this dynamic image widget.
	 *
	 * @param md the medium breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentDynamicImage md(int md) {
		image.setMd(Integer.valueOf(md));
		return this;
	}

	/**
	 * Sets the large breakpoint column span for this dynamic image widget.
	 *
	 * @param lg the large breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentDynamicImage lg(int lg) {
		image.setLg(Integer.valueOf(lg));
		return this;
	}

	/**
	 * Sets the extra-large breakpoint column span for this dynamic image widget.
	 *
	 * @param xl the extra-large breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentDynamicImage xl(int xl) {
		image.setXl(Integer.valueOf(xl));
		return this;
	}

	/**
	 * Appends an image parameter definition to the wrapped metadata.
	 */
	public FluentDynamicImage addParameter(FluentParameter parameter) {
		image.getParameters().add(parameter.get());
		return this;
	}

	/**
	 * Returns the wrapped {@link DynamicImage} metadata instance.
	 *
	 * @return the mutable dynamic-image metadata being configured
	 */
	@Override
	public DynamicImage get() {
		return image;
	}
}
