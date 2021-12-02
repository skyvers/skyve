package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.Label;

public class FluentLabel extends FluentBoundWidget<FluentLabel> {
	private Label label = null;
	
	public FluentLabel() {
		label = new Label();
	}

	public FluentLabel(Label label) {
		this.label = label;
	}
	
	public FluentLabel from(@SuppressWarnings("hiding") Label label) {
		super.from(label);
		return this;
	}

	@Override
	public Label get() {
		return label;
	}
}
