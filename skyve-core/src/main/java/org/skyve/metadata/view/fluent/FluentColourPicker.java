package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;

import jakarta.annotation.Nonnull;

/**
 * Builds {@link ColourPicker} widget metadata using a fluent API.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
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
		colour.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));
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
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentColourPicker putProperty(@Nonnull String k, @Nonnull String v) {
		colour.getProperties().put(k, v);
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
