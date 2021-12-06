package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Combo;

public class FluentCombo extends FluentChangeableInputWidget<FluentCombo> implements FluentAbsoluteWidth<FluentCombo> {
	private Combo combo = null;
	
	public FluentCombo() {
		combo = new Combo();
	}

	public FluentCombo(Combo combo) {
		this.combo = combo;
	}

	public FluentCombo from(@SuppressWarnings("hiding") Combo combo) {

		absoluteWidth(combo, this);

		super.from(combo);
		return this;
	}


	@Override
	public FluentCombo pixelWidth(int width) {
		combo.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public Combo get() {
		return combo;
	}

}
