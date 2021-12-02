package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Radio;

public class FluentRadio extends FluentChangeableInputWidget<FluentRadio> {
	private Radio radio = null;
	
	public FluentRadio() {
		radio = new Radio();
	}

	public FluentRadio(Radio radio) {
		this.radio = radio;
	}

	public FluentRadio from(@SuppressWarnings("hiding") Radio radio) {
		super.from(radio);
		return this;
	}

	@Override
	public Radio get() {
		return radio;
	}
}
