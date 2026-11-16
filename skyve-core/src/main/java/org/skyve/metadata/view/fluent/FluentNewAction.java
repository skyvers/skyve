package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.NewAction;

import jakarta.annotation.Nonnull;

/**
 * Builds {@link NewAction} metadata using a fluent API.
 */
public class FluentNewAction extends FluentAction<FluentNewAction> {
	private NewAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link NewAction} instance.
	 */
	public FluentNewAction() {
		action = new NewAction();
	}

	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentNewAction(@Nonnull NewAction action) {
		this.action = action;
	}

	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public @Nonnull FluentNewAction from(@SuppressWarnings("hiding") @Nonnull NewAction action) {
		super.fromBase(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public @Nonnull NewAction get() {
		return action;
	}
}
