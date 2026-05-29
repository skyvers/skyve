package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.CancelAction;

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
	public FluentCancelAction(CancelAction action) {
		this.action = action;
	}

	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentCancelAction from(@SuppressWarnings("hiding") CancelAction action) {
		super.fromBase(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public CancelAction get() {
		return action;
	}
}
