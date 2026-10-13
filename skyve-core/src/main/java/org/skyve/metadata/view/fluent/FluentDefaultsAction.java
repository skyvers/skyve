package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.DefaultsAction;

import jakarta.annotation.Nonnull;

/**
 * Builds {@link DefaultsAction} metadata using a fluent API.
 */
public class FluentDefaultsAction extends FluentAction<FluentDefaultsAction> {
	private DefaultsAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link DefaultsAction} instance.
	 */
	public FluentDefaultsAction() {
		action = new DefaultsAction();
	}

	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentDefaultsAction(@Nonnull DefaultsAction action) {
		this.action = action;
	}

	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public @Nonnull FluentDefaultsAction from(@SuppressWarnings("hiding") @Nonnull DefaultsAction action) {
		super.fromBase(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public @Nonnull DefaultsAction get() {
		return action;
	}
}
