package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.CancelAction;

import jakarta.annotation.Nonnull;

/**
 * Builds {@link CancelAction} metadata using a fluent API.
 */
public class FluentCancelAction extends FluentAction<FluentCancelAction> {
	private CancelAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link CancelAction} instance.
	 */
	public FluentCancelAction() {
		action = new CancelAction();
	}

	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentCancelAction(@Nonnull CancelAction action) {
		this.action = action;
	}

	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public @Nonnull FluentCancelAction from(@SuppressWarnings("hiding") @Nonnull CancelAction action) {
		super.fromBase(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public @Nonnull CancelAction get() {
		return action;
	}
}
