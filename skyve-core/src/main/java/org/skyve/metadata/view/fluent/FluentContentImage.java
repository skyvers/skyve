package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;

/**
 * Builds {@link ContentImage} widget metadata using a fluent API.
 */
public class FluentContentImage extends FluentInputWidget<FluentContentImage> implements FluentRelativeSize<FluentContentImage> {
	private ContentImage image = null;

	/**
	 * Creates a fluent builder backed by a new {@link ContentImage} metadata instance.
	 */
	public FluentContentImage() {
		image = new ContentImage();
	}

	/**
	 * Creates a fluent builder backed by the supplied content-image metadata instance.
	 *
	 * @param image the metadata instance to mutate
	 */
	public FluentContentImage(ContentImage image) {
		this.image = image;
	}

	/**
	 * Copies content-image metadata into this fluent builder.
	 */
	public FluentContentImage from(@SuppressWarnings("hiding") ContentImage image) {
		Boolean b = image.getEditable();
		if (b != null) {
			editable(b.booleanValue());
		}

		b = image.getShowMarkup();
		if (b != null) {
			showMarkup(b.booleanValue());
		}

		relativeSize(image, this);

		super.from(image);
		return this;
	}

	/**
	 * Sets whether this content-image widget is editable.
	 *
	 * @param editable {@code true} to allow editing
	 * @return this builder
	 */
	public FluentContentImage editable(boolean editable) {
		image.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether markup is shown on this content-image widget.
	 *
	 * @param showMarkup {@code true} to show markup
	 * @return this builder
	 */
	public FluentContentImage showMarkup(boolean showMarkup) {
		image.setShowMarkup(showMarkup ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the absolute pixel height of this content-image widget.
	 *
	 * @param height the height in pixels
	 * @return this builder
	 */
	@Override
	public FluentContentImage pixelHeight(int height) {
		image.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Sets the absolute pixel width of this content-image widget.
	 *
	 * @param width the width in pixels
	 * @return this builder
	 */
	@Override
	public FluentContentImage pixelWidth(int width) {
		image.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the minimum pixel width of this content-image widget.
	 *
	 * @param minPixelWidth the minimum width in pixels
	 * @return this builder
	 */
	@Override
	public FluentContentImage minPixelWidth(int minPixelWidth) {
		image.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel width of this content-image widget.
	 *
	 * @param maxPixelWidth the maximum width in pixels
	 * @return this builder
	 */
	@Override
	public FluentContentImage maxPixelWidth(int maxPixelWidth) {
		image.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel height of this content-image widget.
	 *
	 * @param maxPixelHeight the maximum height in pixels
	 * @return this builder
	 */
	@Override
	public FluentContentImage maxPixelHeight(int maxPixelHeight) {
		image.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	/**
	 * Sets the minimum pixel height of this content-image widget.
	 *
	 * @param minPixelHeight the minimum height in pixels
	 * @return this builder
	 */
	@Override
	public FluentContentImage minPixelHeight(int minPixelHeight) {
		image.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the percentage width of this content-image widget.
	 *
	 * @param percentageWidth the width as a percentage
	 * @return this builder
	 */
	@Override
	public FluentContentImage percentageWidth(int percentageWidth) {
		image.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	/**
	 * Sets the percentage height of this content-image widget.
	 *
	 * @param percentageHeight the height as a percentage
	 * @return this builder
	 */
	@Override
	public FluentContentImage percentageHeight(int percentageHeight) {
		image.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	/**
	 * Sets the responsive width breakpoint for this content-image widget.
	 *
	 * @param responsiveWidth the responsive width value
	 * @return this builder
	 */
	@Override
	public FluentContentImage responsiveWidth(int responsiveWidth) {
		image.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	/**
	 * Sets the small (sm) responsive column span for this content-image widget.
	 *
	 * @param sm the column span at the sm breakpoint
	 * @return this builder
	 */
	@Override
	public FluentContentImage sm(int sm) {
		image.setSm(Integer.valueOf(sm));
		return this;
	}

	/**
	 * Sets the medium (md) responsive column span for this content-image widget.
	 *
	 * @param md the column span at the md breakpoint
	 * @return this builder
	 */
	@Override
	public FluentContentImage md(int md) {
		image.setMd(Integer.valueOf(md));
		return this;
	}

	/**
	 * Sets the large (lg) responsive column span for this content-image widget.
	 *
	 * @param lg the column span at the lg breakpoint
	 * @return this builder
	 */
	@Override
	public FluentContentImage lg(int lg) {
		image.setLg(Integer.valueOf(lg));
		return this;
	}

	/**
	 * Sets the extra-large (xl) responsive column span for this content-image widget.
	 *
	 * @param xl the column span at the xl breakpoint
	 * @return this builder
	 */
	@Override
	public FluentContentImage xl(int xl) {
		image.setXl(Integer.valueOf(xl));
		return this;
	}

	/**
	 * Returns the wrapped content-image metadata instance.
	 *
	 * @return the mutable content-image metadata being configured
	 */
	@Override
	public ContentImage get() {
		return image;
	}
}
