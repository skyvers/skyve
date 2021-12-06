package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;

public class FluentColourPicker extends FluentChangeableInputWidget<FluentColourPicker>
		implements FluentAbsoluteWidth<FluentColourPicker> {
	private ColourPicker colour = null;

	public FluentColourPicker() {
		colour = new ColourPicker();
	}

	public FluentColourPicker(ColourPicker colour) {
		this.colour = colour;
	}

	public FluentColourPicker from(@SuppressWarnings("hiding") ColourPicker colour) {

		absoluteWidth(colour, this);

		super.from(colour);
		return this;
	}

	@Override
	public FluentColourPicker pixelWidth(int pixelWidth) {
		colour.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	@Override
	public ColourPicker get() {
		return colour;
	}

}
