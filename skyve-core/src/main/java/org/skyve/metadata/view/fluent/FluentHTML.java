package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.metadata.view.TextOutput.Sanitisation;

public class FluentHTML extends FluentInputWidget<FluentHTML> implements FluentAbsoluteSize<FluentHTML> {
	private HTML html = null;

	public FluentHTML() {
		html = new HTML();
	}

	public FluentHTML(HTML html) {
		this.html = html;
	}

	public FluentHTML from(@SuppressWarnings("hiding") HTML html) {

		sanitise(html.getSanitise());

		absoluteSize(html, this);

		super.from(html);
		return this;
	}

	public FluentHTML sanitise(Sanitisation sanitise) {
		html.setSanitise(sanitise);
		return this;
	}

	@Override
	public FluentHTML pixelWidth(int width) {
		html.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	@Override
	public FluentHTML pixelHeight(int height) {
		html.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	@Override
	public HTML get() {
		return html;
	}

}
