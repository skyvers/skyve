package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Spinner;

public class FluentSpinner extends FluentWidget {
	private Spinner spinner = null;
	
	public FluentSpinner() {
		spinner = new Spinner();
	}

	public FluentSpinner(Spinner spinner) {
		this.spinner = spinner;
	}

	public FluentSpinner from(@SuppressWarnings("hiding") Spinner spinner) {
		return this;
	}

	@Override
	public Spinner get() {
		return spinner;
	}
}
