package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.TextField;

public class FluentTextField extends FluentChangeableInputWidget<FluentTextField> {
	private TextField text = null;
	
	public FluentTextField() {
		text = new TextField();
	}

	public FluentTextField(TextField text) {
		this.text = text;
	}

	public FluentTextField from(@SuppressWarnings("hiding") TextField text) {
		super.from(text);
		return this;
	}

	@Override
	public TextField get() {
		return text;
	}
}
