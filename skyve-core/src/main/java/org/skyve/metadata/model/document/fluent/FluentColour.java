package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Colour;

public class FluentColour extends FluentConvertableField<FluentColour> {
	private Colour colour = null;
	
	public FluentColour() {
		colour = new Colour();
	}

	public FluentColour(Colour colour) {
		this.colour = colour;
	}

	public FluentColour from(@SuppressWarnings("hiding") Colour colour) {
		super.from(colour);
		return this;
	}
	
	@Override
	public Colour get() {
		return colour;
	}
}
