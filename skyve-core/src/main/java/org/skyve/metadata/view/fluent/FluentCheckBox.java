package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;

public class FluentCheckBox extends FluentChangeableInputWidget<FluentCheckBox> {
	private CheckBox check = null;
	
	public FluentCheckBox() {
		check = new CheckBox();
	}

	public FluentCheckBox(CheckBox check) {
		this.check = check;
	}

	public FluentCheckBox from(@SuppressWarnings("hiding") CheckBox check) {
		super.from(check);
		return this;
	}

	@Override
	public CheckBox get() {
		return check;
	}
}
