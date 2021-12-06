package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.metadata.view.widget.bound.input.KeyboardType;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;

public class FluentTextField extends FluentChangeableInputWidget<FluentTextField> implements FluentAbsoluteWidth<FluentTextField> {
	private TextField text = null;

	public FluentTextField() {
		text = new TextField();
	}

	public FluentTextField(TextField text) {
		this.text = text;
	}

	public FluentTextField from(@SuppressWarnings("hiding") TextField text) {
		editable(text.getEditable());
		keyboardType(text.getKeyboardType());
		complete(text.getComplete());

		absoluteWidth(text, this);

		super.from(text);
		return this;
	}

	public FluentTextField editable(boolean editable) {
		text.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentTextField keyboardType(KeyboardType keyboardType) {
		text.setKeyboardType(keyboardType);
		return this;
	}

	public FluentTextField complete(CompleteType complete) {
		text.setComplete(complete);
		return this;
	}

	@Override
	public FluentTextField pixelWidth(int width) {
		text.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public TextField get() {
		return text;
	}

}
