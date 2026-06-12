package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.metadata.view.TextOutput.Sanitisation;

/**
 * Builds {@link RichText} widget metadata using a fluent API.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class FluentRichText extends FluentChangeableInputWidget<FluentRichText>
		implements FluentAbsoluteSize<FluentRichText>, FluentConstrainableHeight<FluentRichText> {
	private RichText text = null;

	/**
	 * Creates a fluent builder backed by a new {@link RichText} metadata instance.
	 */
	public FluentRichText() {
		text = new RichText();
	}

	/**
	 * Creates a fluent builder backed by the supplied {@link RichText} metadata instance.
	 *
	 * @param text the metadata instance to mutate
	 */
	public FluentRichText(RichText text) {
		this.text = text;
	}

	/**
	 * Copies rich-text metadata into this fluent builder.
	 *
	 * @param text
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentRichText from(@SuppressWarnings("hiding") RichText text) {

		sanitise(text.getSanitise());

		absoluteSize(text, this);

		constrainableHeight(text, this);

		super.from(text);
		return this;
	}

	/**
	 * Sets the sanitisation policy for this rich-text widget.
	 *
	 * @param sanitise the sanitisation policy to apply
	 * @return this builder
	 */
	public FluentRichText sanitise(Sanitisation sanitise) {
		text.setSanitise(sanitise);
		return this;
	}

	/**
	 * Sets the pixel width of this rich-text widget.
	 *
	 * @param width the pixel width
	 * @return this builder
	 */
	@Override
	public FluentRichText pixelWidth(int width) {
		text.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the pixel height of this rich-text widget.
	 *
	 * @param height the pixel height
	 * @return this builder
	 */
	@Override
	public FluentRichText pixelHeight(int height) {
		text.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Sets the minimum pixel height of this rich-text widget.
	 *
	 * @param minPixelHeight the minimum pixel height
	 * @return this builder
	 */
	@Override
	public FluentRichText minPixelHeight(int minPixelHeight) {
		text.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	/**
	 * Sets the maximum pixel height of this rich-text widget.
	 *
	 * @param maxPixelHeight the maximum pixel height
	 * @return this builder
	 */
	@Override
	public FluentRichText maxPixelHeight(int maxPixelHeight) {
		text.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	/**
	 * Returns the wrapped {@link RichText} metadata instance.
	 *
	 * @return the mutable rich-text metadata being configured
	 */
	@Override
	public RichText get() {
		return text;
	}
}
