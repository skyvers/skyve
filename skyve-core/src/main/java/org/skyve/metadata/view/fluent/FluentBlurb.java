package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.metadata.view.TextOutput.Sanitisation;

/**
 * Builds {@link Blurb} widget metadata using a fluent API.
 */
public class FluentBlurb extends FluentWidget implements FluentAbsoluteSize<FluentBlurb> {
	private Blurb blurb = null;

	/**
	 * Creates a fluent builder backed by a new {@link Blurb} metadata instance.
	 */
	public FluentBlurb() {
		blurb = new Blurb();
	}

	/**
	 * Creates a fluent builder backed by the supplied blurb metadata instance.
	 *
	 * @param blurb the metadata instance to mutate
	 */
	public FluentBlurb(Blurb blurb) {
		this.blurb = blurb;
	}

	/**
	 * Copies blurb metadata into this fluent builder.
	 */
	public FluentBlurb from(@SuppressWarnings("hiding") Blurb blurb) {

		markup(blurb.getMarkup());

		absoluteSize(blurb, this);

		invisibleConditionName(blurb.getInvisibleConditionName());
		textAlignment(blurb.getTextAlignment());
		Boolean b = blurb.getEscape();
		if (b != null) {
			escape(b.booleanValue());
		}
		sanitise(blurb.getSanitise());

		blurb.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));
		return this;
	}

	/**
	 * Adds or replaces a custom property on the wrapped blurb metadata.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public FluentBlurb putProperty(String k, String v) {
		blurb.getProperties().put(k, v);
		return this;
	}

	/**
	 * Sets the markup text rendered by the blurb.
	 *
	 * @param markup the markup content
	 * @return this builder
	 */
	public FluentBlurb markup(String markup) {
		blurb.setMarkup(markup);
		return this;
	}

	/**
	 * Sets the absolute pixel width for this blurb widget.
	 *
	 * @param pixelWidth the width in pixels
	 * @return this builder
	 */
	@Override
	public FluentBlurb pixelWidth(int pixelWidth) {
		blurb.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	/**
	 * Sets the absolute pixel height for this blurb widget.
	 *
	 * @param pixelHeight the height in pixels
	 * @return this builder
	 */
	@Override
	public FluentBlurb pixelHeight(int pixelHeight) {
		blurb.setPixelHeight(Integer.valueOf(pixelHeight));
		return this;
	}

	/**
	 * Sets the condition name used to hide this blurb when it evaluates to true.
	 *
	 * @param invisibleConditionName the invisibility condition name
	 * @return this builder
	 */
	public FluentBlurb invisibleConditionName(String invisibleConditionName) {
		blurb.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	/**
	 * Sets the horizontal text alignment for the blurb markup.
	 *
	 * @param textAlignment the alignment to apply
	 * @return this builder
	 */
	public FluentBlurb textAlignment(HorizontalAlignment textAlignment) {
		blurb.setTextAlignment(textAlignment);
		return this;
	}

	/**
	 * Controls whether markup content is escaped before rendering.
	 *
	 * @param escape true to escape markup, false to render markup
	 * @return this builder
	 */
	public FluentBlurb escape(boolean escape) {
		blurb.setEscape(escape ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the sanitisation strategy applied before rendering the blurb content.
	 *
	 * @param sanitise the sanitisation strategy
	 * @return this builder
	 */
	public FluentBlurb sanitise(Sanitisation sanitise) {
		blurb.setSanitise(sanitise);
		return this;
	}

	/**
	 * Returns the wrapped blurb metadata instance.
	 *
	 * @return the mutable blurb metadata being configured
	 */
	@Override
	public Blurb get() {
		return blurb;
	}
}
