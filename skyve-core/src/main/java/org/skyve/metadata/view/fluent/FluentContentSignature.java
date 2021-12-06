package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;

public class FluentContentSignature extends FluentInputWidget<FluentContentSignature>
		implements FluentAbsoluteSize<FluentContentSignature> {
	private ContentSignature signature = null;

	public FluentContentSignature() {
		signature = new ContentSignature();
	}

	public FluentContentSignature(ContentSignature signature) {
		this.signature = signature;
	}

	public FluentContentSignature from(@SuppressWarnings("hiding") ContentSignature signature) {

		rgbHexBackgroundColour(signature.getRgbHexBackgroundColour());
		rgbHexForegroundColour(signature.getRgbHexForegroundColour());

		absoluteSize(signature, this);

		super.from(signature);
		return this;
	}

	public FluentContentSignature rgbHexBackgroundColour(String rgbHexBackgroundColour) {
		signature.setRgbHexBackgroundColour(rgbHexBackgroundColour);
		return this;
	}

	public FluentContentSignature rgbHexForegroundColour(String rgbHexForegroundColour) {
		signature.setRgbHexForegroundColour(rgbHexForegroundColour);
		return this;
	}

	@Override
	public FluentContentSignature pixelWidth(int width) {
		signature.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentContentSignature pixelHeight(int height) {
		signature.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public ContentSignature get() {
		return signature;
	}
}
