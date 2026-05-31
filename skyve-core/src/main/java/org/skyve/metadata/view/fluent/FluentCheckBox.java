package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;

/**
 * Builds {@link CheckBox} widget metadata using a fluent API.
 */
public class FluentCheckBox extends FluentChangeableInputWidget<FluentCheckBox> implements FluentAbsoluteSize<FluentCheckBox> {
	private CheckBox check = null;

	/**
	 * Creates a fluent builder backed by a new {@link CheckBox} metadata instance.
	 */
	public FluentCheckBox() {
		check = new CheckBox();
	}

	/**
	 * Creates a fluent builder backed by the supplied check-box metadata instance.
	 *
	 * @param check the metadata instance to mutate
	 */
	public FluentCheckBox(CheckBox check) {
		this.check = check;
	}

	/**
	 * Copies check-box metadata into this fluent builder.
	 */
	public FluentCheckBox from(@SuppressWarnings("hiding") CheckBox check) {
		Boolean b = check.getTriState();
		if (b != null) {
			triState(b.booleanValue());
		}

		absoluteSize(check, this);

		super.from(check);
		return this;
	}

	/**
	 * Sets the absolute pixel width for this check-box widget.
	 *
	 * @param width the width in pixels
	 * @return this builder
	 */
	@Override
	public FluentCheckBox pixelWidth(int width) {
		check.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Sets the absolute pixel height for this check-box widget.
	 *
	 * @param height the height in pixels
	 * @return this builder
	 */
	@Override
	public FluentCheckBox pixelHeight(int height) {
		check.setPixelHeight(Integer.valueOf(height));
		return this;
	}

	/**
	 * Enables or disables tri-state behavior for this check box.
	 *
	 * @param triState true to allow indeterminate state, false for binary state only
	 * @return this builder
	 */
	public FluentCheckBox triState(boolean triState) {
		check.setTriState(triState ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Returns the wrapped check-box metadata instance.
	 *
	 * @return the mutable check-box metadata being configured
	 */
	@Override
	public CheckBox get() {
		return check;
	}

}
