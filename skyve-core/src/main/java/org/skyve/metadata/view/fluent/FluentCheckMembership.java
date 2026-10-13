package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;

import jakarta.annotation.Nonnull;

/**
 * Builds {@link CheckMembership} widget metadata using a fluent API.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class FluentCheckMembership extends FluentChangeableInputWidget<FluentCheckMembership> {
	private CheckMembership check = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link CheckMembership} metadata instance.
	 */
	public FluentCheckMembership() {
		check = new CheckMembership();
	}

	/**
	 * Creates a fluent builder backed by the supplied membership metadata instance.
	 *
	 * @param check the metadata instance to mutate
	 */
	public FluentCheckMembership(CheckMembership check) {
		this.check = check;
	}

	/**
	 * Copies check-membership metadata into this fluent builder.
	 */
	public FluentCheckMembership from(@SuppressWarnings("hiding") CheckMembership check) {
		super.from(check);
		check.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));
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
	public @Nonnull FluentCheckMembership putProperty(@Nonnull String k, @Nonnull String v) {
		check.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the wrapped membership metadata instance.
	 *
	 * @return the mutable membership metadata being configured
	 */
	@Override
	public CheckMembership get() {
		return check;
	}
}
