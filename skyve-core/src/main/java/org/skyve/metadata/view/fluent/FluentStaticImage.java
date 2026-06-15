package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.StaticImage;

/**
 * Builds {@link StaticImage} widget metadata using a fluent API.
 */
public class FluentStaticImage extends FluentWidget implements FluentRelativeSize<FluentStaticImage> {
	private StaticImage image = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link StaticImage} metadata instance.
	 */
	public FluentStaticImage() {
		this.image = new StaticImage();
	}
	
	/**
	 * Creates a fluent builder backed by the supplied {@link StaticImage} metadata instance.
	 *
	 * @param image the metadata instance to mutate
	 */
	public FluentStaticImage(StaticImage image) {
		this.image = image;
	}
	
	/**
	 * Copies static-image metadata into this fluent builder.
	 */
	public FluentStaticImage from(@SuppressWarnings("hiding") StaticImage image) {

		relativeFile(image.getRelativeFile());

		relativeSize(image, this);

		invisibleConditionName(image.getInvisibleConditionName());
		
		return this;
	}
	
	/**
	 * Sets the relative file path for this static image.
	 *
	 * @param relativeFile the relative file path to the image
	 * @return this builder
	 */
	public FluentStaticImage relativeFile(String relativeFile) {
		image.setRelativeFile(relativeFile);
		return this;
	}
	
	/**
	 * Sets the pixel width of this static image.
	 *
	 * @param pixelWidth the pixel width
	 * @return this builder
	 */
	@Override
	public FluentStaticImage pixelWidth(int pixelWidth) {
		image.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	/**
	 * Sets the responsive width breakpoint for this static image.
	 *
	 * @param responsiveWidth the responsive width value
	 * @return this builder
	 */
	@Override
	public FluentStaticImage responsiveWidth(int responsiveWidth) {
		image.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	/**
	 * Sets the small breakpoint column span for this static image.
	 *
	 * @param sm the small breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentStaticImage sm(int sm) {
		image.setSm(Integer.valueOf(sm));
		return this;
	}
	
	/**
	 * Sets the medium breakpoint column span for this static image.
	 *
	 * @param md the medium breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentStaticImage md(int md) {
		image.setMd(Integer.valueOf(md));
		return this;
	}
	
	/**
	 * Sets the large breakpoint column span for this static image.
	 *
	 * @param lg the large breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentStaticImage lg(int lg) {
		image.setLg(Integer.valueOf(lg));
		return this;
	}

	/**
	 * Sets the extra-large breakpoint column span for this static image.
	 *
	 * @param xl the extra-large breakpoint width
	 * @return this builder
	 */
	@Override
	public FluentStaticImage xl(int xl) {
		image.setXl(Integer.valueOf(xl));
		return this;
	}

	/**
	 * Sets the percentage width of this static image.
	 *
	 * @param percentageWidth the percentage width
	 * @return this builder
	 */
	@Override
	public FluentStaticImage percentageWidth(int percentageWidth) {
		image.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	/**
	 * Sets the minimum pixel width of this static image.
	 *
	 * @param minPixelWidth the minimum pixel width
	 * @return this builder
	 */
	@Override
	public FluentStaticImage minPixelWidth(int minPixelWidth) {
		image.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel width of this static image.
	 *
	 * @param maxPixelWidth the maximum pixel width
	 * @return this builder
	 */
	@Override
	public FluentStaticImage maxPixelWidth(int maxPixelWidth) {
		image.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	/**
	 * Sets the pixel height of this static image.
	 *
	 * @param pixelHeight the pixel height
	 * @return this builder
	 */
	@Override
	public FluentStaticImage pixelHeight(int pixelHeight) {
		image.setPixelHeight(Integer.valueOf(pixelHeight));
		return this;
	}

	/**
	 * Sets the percentage height of this static image.
	 *
	 * @param percentageHeight the percentage height
	 * @return this builder
	 */
	@Override
	public FluentStaticImage percentageHeight(int percentageHeight) {
		image.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	/**
	 * Sets the minimum pixel height of this static image.
	 *
	 * @param minPixelHeight the minimum pixel height
	 * @return this builder
	 */
	@Override
	public FluentStaticImage minPixelHeight(int minPixelHeight) {
		image.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the maximum pixel height of this static image.
	 *
	 * @param maxPixelHeight the maximum pixel height
	 * @return this builder
	 */
	@Override
	public FluentStaticImage maxPixelHeight(int maxPixelHeight) {
		image.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}
	
	/**
	 * Sets the condition name that hides this static image.
	 *
	 * @param invisibleConditionName the condition name
	 * @return this builder
	 */
	public FluentStaticImage invisibleConditionName(String invisibleConditionName) {
		image.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	/**
	 * Returns the wrapped {@link StaticImage} metadata instance.
	 *
	 * @return the mutable static-image metadata being configured
	 */
	@Override
	public StaticImage get() {
		return image;
	}
}
