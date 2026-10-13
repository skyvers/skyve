package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Combo;

import jakarta.annotation.Nonnull;

/**
 * Builds {@link Combo} widget metadata using a fluent API.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class FluentCombo extends FluentChangeableInputWidget<FluentCombo> implements FluentAbsoluteWidth<FluentCombo> {
	private Combo combo = null;

	/**
	 * Creates a fluent builder backed by a new {@link Combo} metadata instance.
	 */
	public FluentCombo() {
		combo = new Combo();
	}

	/**
	 * Creates a fluent builder backed by the supplied combo metadata instance.
	 *
	 * @param combo the metadata instance to mutate
	 */
	public FluentCombo(Combo combo) {
		this.combo = combo;
	}

	/**
	 * Copies combo metadata into this fluent builder.
	 */
	public FluentCombo from(@SuppressWarnings("hiding") Combo combo) {
		absoluteWidth(combo, this);
		super.from(combo);
		combo.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return this;
	}

	/**
	 * Sets the absolute pixel width for this combo widget.
	 *
	 * @param width the width in pixels
	 * @return this builder
	 */
	@Override
	public FluentCombo pixelWidth(int width) {
		combo.setPixelWidth(Integer.valueOf(width));
		return this;
	}

	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentCombo putProperty(@Nonnull String k, @Nonnull String v) {
		combo.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the wrapped combo metadata instance.
	 *
	 * @return the mutable combo metadata being configured
	 */
	@Override
	public Combo get() {
		return combo;
	}
}
