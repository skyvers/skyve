package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.Label;

public class FluentLabel extends FluentWidget {
	private Label label = null;
	
	public FluentLabel() {
		label = new Label();
	}

	public FluentLabel(Label label) {
		this.label = label;
	}
	
	public FluentLabel from(@SuppressWarnings("hiding") Label label) {
		return this;
	}

	@Override
	public Label get() {
		return label;
	}
}
