package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.TextArea;

public class FluentTextArea extends FluentWidget {
	private TextArea text = null;
	
	public FluentTextArea() {
		text = new TextArea();
	}

	public FluentTextArea(TextArea text) {
		this.text = text;
	}

	public FluentTextArea from(@SuppressWarnings("hiding") TextArea text) {
		return this;
	}

	@Override
	public TextArea get() {
		return text;
	}
}
