package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;

/**
 * Builds {@link ColourPicker} widget metadata using a fluent API.
 */
public class FluentColourPicker extends FluentChangeableInputWidget<FluentColourPicker>
		implements FluentAbsoluteWidth<FluentColourPicker> {
	private ColourPicker colour = null;

	/**
	 * Creates a fluent builder backed by a new {@link ColourPicker} metadata instance.
	 */
	public FluentColourPicker() {
		colour = new ColourPicker();
	}

	/**
	 * Creates a fluent builder backed by the supplied colour-picker metadata instance.
	 *
	 * @param colour the metadata instance to mutate
	 */
	public FluentColourPicker(ColourPicker colour) {
		this.colour = colour;
	}

	/**
	 * Copies colour-picker metadata into this fluent builder.
	 */
	public FluentColourPicker from(@SuppressWarnings("hiding") ColourPicker colour) {

		absoluteWidth(colour, this);

		super.from(colour);
		return this;
	}

	/**
	 * Sets the absolute pixel width for this colour-picker widget.
	 *
	 * @param pixelWidth the width in pixels
	 * @return this builder
	 */
	@Override
	public FluentColourPicker pixelWidth(int pixelWidth) {
		colour.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	/**
	 * Returns the wrapped colour-picker metadata instance.
	 *
	 * @return the mutable colour-picker metadata being configured
	 */
	@Override
	public ColourPicker get() {
		return colour;
	}

}
