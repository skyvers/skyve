package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Radio;

public class FluentRadio extends FluentChangeableInputWidget<FluentRadio> implements FluentAbsoluteWidth<FluentRadio> {
	private Radio radio = null;

	public FluentRadio() {
		radio = new Radio();
	}

	public FluentRadio(Radio radio) {
		this.radio = radio;
	}

	public FluentRadio from(@SuppressWarnings("hiding") Radio radio) {

		vertical(radio.getVertical());

		absoluteWidth(radio, this);

		super.from(radio);
		return this;
	}

	public FluentRadio vertical(boolean vertical) {
		radio.setVertical(vertical ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	@Override
	public FluentRadio pixelWidth(int width) {
		radio.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public Radio get() {
		return radio;
	}
}
