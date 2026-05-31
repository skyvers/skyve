package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;

/**
 * Builds {@link ContentSignature} widget metadata using a fluent API.
 */
public class FluentContentSignature extends FluentInputWidget<FluentContentSignature>
		implements FluentAbsoluteSize<FluentContentSignature> {
	private ContentSignature signature = null;

	/**
	 * Creates a fluent builder backed by a new {@link ContentSignature} metadata instance.
	 */
	public FluentContentSignature() {
		signature = new ContentSignature();
	}

	/**
	 * Creates a fluent builder backed by the supplied {@link ContentSignature} metadata instance.
	 *
	 * @param signature the metadata instance to mutate
	 */
	public FluentContentSignature(ContentSignature signature) {
		this.signature = signature;
	}

	/**
	 * Copies signature metadata into this fluent builder.
	 *
	 * @param signature
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentContentSignature from(@SuppressWarnings("hiding") ContentSignature signature) {

		rgbHexBackgroundColour(signature.getRgbHexBackgroundColour());
		rgbHexForegroundColour(signature.getRgbHexForegroundColour());

		absoluteSize(signature, this);

		super.from(signature);
		return this;
	}

	/**
	 * Sets the background colour as an RGB hex string.
	 *
	 * @param rgbHexBackgroundColour the hex colour value
	 * @return this builder
	 */
	public FluentContentSignature rgbHexBackgroundColour(String rgbHexBackgroundColour) {
		signature.setRgbHexBackgroundColour(rgbHexBackgroundColour);
		return this;
	}

	/**
	 * Sets the foreground colour as an RGB hex string.
	 *
	 * @param rgbHexForegroundColour the hex colour value
	 * @return this builder
	 */
	public FluentContentSignature rgbHexForegroundColour(String rgbHexForegroundColour) {
		signature.setRgbHexForegroundColour(rgbHexForegroundColour);
		return this;
	}

	/**
	 * Sets the pixel width of this signature widget.
	 *
	 * @param width the pixel width
	 * @return this builder
	 */
	@Override
	public FluentContentSignature pixelWidth(int width) {
		signature.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the pixel height of this signature widget.
	 *
	 * @param height the pixel height
	 * @return this builder
	 */
	@Override
	public FluentContentSignature pixelHeight(int height) {
		signature.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Returns the wrapped {@link ContentSignature} metadata instance.
	 *
	 * @return the mutable content-signature metadata being configured
	 */
	@Override
	public ContentSignature get() {
		return signature;
	}
}
