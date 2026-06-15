package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Radio;

/**
 * Builds {@link Radio} widget metadata using a fluent API.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class FluentRadio extends FluentChangeableInputWidget<FluentRadio> implements FluentAbsoluteWidth<FluentRadio> {
	private Radio radio = null;

	/**
	 * Creates a fluent builder backed by a new {@link Radio} metadata instance.
	 */
	public FluentRadio() {
		radio = new Radio();
	}

	/**
	 * Creates a fluent builder backed by the supplied {@link Radio} metadata instance.
	 *
	 * @param radio the metadata instance to mutate
	 */
	public FluentRadio(Radio radio) {
		this.radio = radio;
	}

	/**
	 * Copies radio metadata into this fluent builder.
	 */
	public FluentRadio from(@SuppressWarnings("hiding") Radio radio) {
		Boolean b = radio.getVertical();
		if (b != null) {
			vertical(b.booleanValue());
		}

		absoluteWidth(radio, this);

		super.from(radio);
		return this;
	}

	/**
	 * Sets whether the radio buttons are displayed vertically.
	 *
	 * @param vertical {@code true} to display buttons vertically
	 * @return this builder
	 */
	public FluentRadio vertical(boolean vertical) {
		radio.setVertical(vertical ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the pixel width of this radio widget.
	 *
	 * @param width the pixel width
	 * @return this builder
	 */
	@Override
	public FluentRadio pixelWidth(int width) {
		radio.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Returns the wrapped {@link Radio} metadata instance.
	 *
	 * @return the mutable radio metadata being configured
	 */
	@Override
	public Radio get() {
		return radio;
	}
}
