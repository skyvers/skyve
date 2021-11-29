package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Colour;

public class FluentColour extends FluentConvertableField<FluentColour> {
	private Colour colour = new Colour();
	
	public FluentColour() {
		// nothing to see
	}

	public FluentColour(Colour colour) {
		super(colour);
	}
	
	@Override
	public Colour get() {
		return colour;
	}
}
