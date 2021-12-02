package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.RichText;

public class FluentRichText extends FluentChangeableInputWidget<FluentRichText> {
	private RichText text = null;
	
	public FluentRichText() {
		text = new RichText();
	}

	public FluentRichText(RichText text) {
		this.text = text;
	}

	public FluentRichText from(@SuppressWarnings("hiding") RichText text) {
		super.from(text);
		return this;
	}

	@Override
	public RichText get() {
		return text;
	}
}
