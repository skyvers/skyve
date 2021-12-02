package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Comparison;

public class FluentComparison extends FluentInputWidget<FluentComparison> {
	private Comparison comparison = null;

	public FluentComparison() {
		comparison = new Comparison();
	}

	public FluentComparison(Comparison comparison) {
		this.comparison = comparison;
	}

	public FluentComparison from(@SuppressWarnings("hiding") Comparison comparison) {
		super.from(comparison);
		return this;
	}

	@Override
	public Comparison get() {
		return comparison;
	}
}
