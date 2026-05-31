package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Spinner;

/**
 * Builds {@link Spinner} widget metadata using a fluent API.
 */
public class FluentSpinner extends FluentTextField {
	private Spinner spinner = null;

	/**
	 * Creates a builder backed by a new {@link Spinner}.
	 */
	public FluentSpinner() {
		spinner = new Spinner();
	}

	/**
	 * Creates a builder backed by the supplied {@link Spinner}.
	 *
	 * @param spinner
	 *            the metadata instance to mutate
	 */
	public FluentSpinner(Spinner spinner) {
		this.spinner = spinner;
	}

	/**
	 * Copies spinner metadata into this fluent builder.
	 *
	 * @param spinner
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentSpinner from(@SuppressWarnings("hiding") Spinner spinner) {
		Double d = spinner.getMin();
		if (d != null) {
			min(d.doubleValue());
		}
		d = spinner.getMax();
		if (d != null) {
			max(d.doubleValue());
		}
		d = spinner.getStep();
		if (d != null) {
			step(d.doubleValue());
		}

		super.from(spinner);
		return this;
	}

	/**
	 * Sets the minimum value for this spinner.
	 *
	 * @param min the minimum value
	 * @return this builder
	 */
	public FluentSpinner min(double min) {
		spinner.setMin(Double.valueOf(min));
		return this;
	}

	/**
	 * Sets the maximum value for this spinner.
	 *
	 * @param max the maximum value
	 * @return this builder
	 */
	public FluentSpinner max(double max) {
		spinner.setMax(Double.valueOf(max));
		return this;
	}

	/**
	 * Sets the step increment for this spinner.
	 *
	 * @param step the step value
	 * @return this builder
	 */
	public FluentSpinner step(double step) {
		spinner.setStep(Double.valueOf(step));
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped spinner metadata
	 */
	@Override
	public Spinner get() {
		return spinner;
	}
}
