package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.Spacer;

public class FluentSpacer extends FluentWidget {
	private Spacer spacer = null;
	
	public FluentSpacer() {
		spacer = new Spacer();
	}
	
	public FluentSpacer(Spacer spacer) {
		this.spacer = spacer;
	}

	public FluentSpacer from(@SuppressWarnings("hiding") Spacer spacer) {
		return this;
	}

	@Override
	public Spacer get() {
		return spacer;
	}
}
