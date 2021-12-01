package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.ProgressBar;

public class FluentProgressBar extends FluentWidget {
	private ProgressBar bar = null;
	
	public FluentProgressBar() {
		bar = new ProgressBar();
	}
	
	public FluentProgressBar(ProgressBar bar) {
		this.bar = bar;
	}

	public FluentProgressBar from(@SuppressWarnings("hiding") ProgressBar bar) {
		return this;
	}
	
	@Override
	public ProgressBar get() {
		return bar;
	}
}
