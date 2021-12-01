package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;

public class FluentColourPicker extends FluentWidget {
	private ColourPicker colour = null;
	
	public FluentColourPicker() {
		colour = new ColourPicker();
	}

	public FluentColourPicker(ColourPicker colour) {
		this.colour = colour;
	}

	public FluentColourPicker from(ColourPicker colour) {
		return this;
	}
	
	@Override
	public ColourPicker get() {
		return colour;
	}
}
