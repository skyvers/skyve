package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Combo;

public class FluentCombo extends FluentChangeableInputWidget<FluentCombo> {
	private Combo combo = null;
	
	public FluentCombo() {
		combo = new Combo();
	}

	public FluentCombo(Combo combo) {
		this.combo = combo;
	}

	public FluentCombo from(@SuppressWarnings("hiding") Combo combo) {
		super.from(combo);
		return this;
	}

	@Override
	public Combo get() {
		return combo;
	}
}
