package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.HTML;

public class FluentHTML extends FluentInputWidget<FluentHTML> {
	private HTML html = null;
	
	public FluentHTML() {
		html = new HTML();
	}

	public FluentHTML(HTML html) {
		this.html = html;
	}

	public FluentHTML from(@SuppressWarnings("hiding") HTML html) {
		super.from(html);
		return this;
	}

	@Override
	public HTML get() {
		return html;
	}
}
