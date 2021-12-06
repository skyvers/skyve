package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.metadata.view.TextOutput.Sanitisation;

public class FluentRichText extends FluentChangeableInputWidget<FluentRichText>
		implements FluentAbsoluteSize<FluentRichText>, FluentConstrainableHeight<FluentRichText> {
	private RichText text = null;

	public FluentRichText() {
		text = new RichText();
	}

	public FluentRichText(RichText text) {
		this.text = text;
	}

	public FluentRichText from(@SuppressWarnings("hiding") RichText text) {

		sanitise(text.getSanitise());

		absoluteSize(text, this);
		constrainableHeight(text, this);

		super.from(text);
		return this;
	}

	public FluentRichText sanitise(Sanitisation sanitise) {
		text.setSanitise(sanitise);
		return this;
	}

	@Override
	public FluentRichText pixelWidth(int width) {
		text.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentRichText pixelHeight(int height) {
		text.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public FluentRichText minPixelHeight(int minPixelHeight) {
		text.setMinPixelHeight(Integer.valueOf(minPixelHeight));
		return this;
	}

	@Override
	public FluentRichText maxPixelHeight(int maxPixelHeight) {
		text.setMaxPixelHeight(Integer.valueOf(maxPixelHeight));
		return this;
	}

	@Override
	public RichText get() {
		return text;
	}
}
