package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;

/**
 * Builds {@link CheckMembership} widget metadata using a fluent API.
 */
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
