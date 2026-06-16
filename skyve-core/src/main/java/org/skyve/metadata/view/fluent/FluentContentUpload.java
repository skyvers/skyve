package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ContentCapture;
import org.skyve.impl.metadata.view.widget.bound.input.ContentDisplay;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds {@link ContentUpload} metadata using a fluent API.
 *
 * <p>Threading: fluent builders are mutable and not thread-safe.
 */
public class FluentContentUpload extends FluentInputWidget<FluentContentUpload> implements FluentRelativeSize<FluentContentUpload> {
	private ContentUpload content = null;

	/**
	 * Creates a fluent builder backed by a new {@link ContentUpload} metadata instance.
	 */
	public FluentContentUpload() {
		content = new ContentUpload();
	}

	/**
	 * Creates a fluent builder backed by the supplied content metadata instance.
	 *
	 * @param content the metadata instance to mutate
	 */
	public FluentContentUpload(@Nonnull ContentUpload content) {
		this.content = content;
	}

	/**
	 * Copies managed-content upload metadata into this fluent builder.
	 *
	 * @param content the source metadata instance; must not be {@code null}
	 * @return this builder
	 */
	public @Nonnull FluentContentUpload from(@SuppressWarnings("hiding") @Nonnull ContentUpload content) {
		ContentDisplay display = content.getDisplay();
		if (display != null) {
			display(display);
		}
		ContentCapture capture = content.getCapture();
		if (capture != null) {
			capture(capture);
		}
		Boolean b = content.getEditable();
		if (b != null) {
			editable(b.booleanValue());
		}
		b = content.getShowMarkup();
		if (b != null) {
			showMarkup(b.booleanValue());
		}
		relativeSize(content, this);

		super.from(content);
		return this;
	}

	/**
	 * Sets the managed-content presentation mode.
	 *
	 * @param display the display mode, or {@code null} to leave the XML attribute
	 *        unset and use {@link ContentUpload#getResolvedDisplay()}
	 * @return this builder
	 */
	public @Nonnull FluentContentUpload display(@Nullable ContentDisplay display) {
		content.setDisplay(display);
		return this;
	}

	/**
	 * Sets the managed-content capture affordance.
	 *
	 * @param capture the capture affordance, or {@code null} to leave the XML
	 *        attribute unset and use {@link ContentUpload#getResolvedCapture()}
	 * @return this builder
	 */
	public @Nonnull FluentContentUpload capture(@Nullable ContentCapture capture) {
		content.setCapture(capture);
		return this;
	}

	/**
	 * Sets whether the widget is explicitly editable.
	 *
	 * @param editable {@code true} to force editability, {@code false} to force
	 *        read-only behaviour
	 * @return this builder
	 */
	public @Nonnull FluentContentUpload editable(boolean editable) {
		content.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether image markup controls are requested.
	 *
	 * @param showMarkup {@code true} to request markup controls, otherwise
	 *        {@code false}
	 * @return this builder
	 */
	public @Nonnull FluentContentUpload showMarkup(boolean showMarkup) {
		content.setShowMarkup(showMarkup ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the fixed pixel height.
	 *
	 * @param height the pixel height to write to the metadata
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentContentUpload pixelHeight(int height) {
		content.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Sets the fixed pixel width.
	 *
	 * @param width the pixel width to write to the metadata
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentContentUpload pixelWidth(int width) {
		content.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the minimum pixel width.
	 *
	 * @param minPixelWidth the minimum pixel width to write to the metadata
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentContentUpload minPixelWidth(int minPixelWidth) {
		content.setMinPixelWidth(Integer.valueOf(minPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel width.
	 *
	 * @param maxPixelWidth the maximum pixel width to write to the metadata
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentContentUpload maxPixelWidth(int maxPixelWidth) {
		content.setMaxPixelWidth(Integer.valueOf(maxPixelWidth));
		return this;
	}

	/**
	 * Sets the maximum pixel height.
	 *
	 * @param maxPixelHeight the maximum pixel height to write to the metadata
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentContentUpload maxPixelHeight(int maxPixelHeight) {
		content.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	/**
	 * Sets the minimum pixel height.
	 *
	 * @param minPixelHeight the minimum pixel height to write to the metadata
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentContentUpload minPixelHeight(int minPixelHeight) {
		content.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the percentage width.
	 *
	 * @param percentageWidth the percentage width to write to the metadata
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentContentUpload percentageWidth(int percentageWidth) {
		content.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	/**
	 * Sets the percentage height.
	 *
	 * @param percentageHeight the percentage height to write to the metadata
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentContentUpload percentageHeight(int percentageHeight) {
		content.setPercentageHeight(Integer.valueOf(percentageHeight));
		return this;
	}

	/**
	 * Sets the responsive grid width.
	 *
	 * @param responsiveWidth the responsive width to write to the metadata
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentContentUpload responsiveWidth(int responsiveWidth) {
		content.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	/**
	 * Sets the small-breakpoint responsive width.
	 *
	 * @param sm the small-breakpoint width to write to the metadata
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentContentUpload sm(int sm) {
		content.setSm(Integer.valueOf(sm));
		return this;
	}

	/**
	 * Sets the medium-breakpoint responsive width.
	 *
	 * @param md the medium-breakpoint width to write to the metadata
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentContentUpload md(int md) {
		content.setMd(Integer.valueOf(md));
		return this;
	}

	/**
	 * Sets the large-breakpoint responsive width.
	 *
	 * @param lg the large-breakpoint width to write to the metadata
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentContentUpload lg(int lg) {
		content.setLg(Integer.valueOf(lg));
		return this;
	}

	/**
	 * Sets the extra-large-breakpoint responsive width.
	 *
	 * @param xl the extra-large-breakpoint width to write to the metadata
	 * @return this builder
	 */
	@Override
	public @Nonnull FluentContentUpload xl(int xl) {
		content.setXl(Integer.valueOf(xl));
		return this;
	}

	/**
	 * Returns the metadata instance being built.
	 *
	 * @return the mutable backing {@link ContentUpload}; never {@code null}
	 */
	@Override
	public @Nonnull ContentUpload get() {
		return content;
	}
}
