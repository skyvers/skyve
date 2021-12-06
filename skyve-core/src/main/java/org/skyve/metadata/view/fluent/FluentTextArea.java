package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.KeyboardType;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;

public class FluentTextArea extends FluentChangeableInputWidget<FluentTextArea> implements FluentAbsoluteSize<FluentTextArea> {
	private TextArea text = null;

	public FluentTextArea() {
		text = new TextArea();
	}

	public FluentTextArea(TextArea text) {
		this.text = text;
	}

	public FluentTextArea from(@SuppressWarnings("hiding") TextArea text) {
		wordWrap(text.getWordWrap());
		editable(text.getEditable());

		minPixelHeight(text.getMinPixelHeight());

		keyboardType(text.getKeyboardType());

		absoluteSize(text, this);

		super.from(text);
		return this;
	}

	public FluentTextArea wordWrap(boolean wordWrap) {
		text.setWordWrap(wordWrap ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentTextArea editable(boolean editable) {
		text.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentTextArea minPixelHeight(int minPixelHeight) {
		text.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return null;
	}

	public FluentTextArea keyboardType(KeyboardType keyboardType) {
		text.setKeyboardType(keyboardType);
		return null;
	}

	@Override
	public FluentTextArea pixelWidth(int width) {
		text.setPixelWidth(Integer.valueOf(width));
		return null;
	}

	@Override
	public FluentTextArea pixelHeight(int height) {
		text.setPixelHeight(Integer.valueOf(height));
		return null;
	}

	@Override
	public TextArea get() {
		return text;
	}
}
