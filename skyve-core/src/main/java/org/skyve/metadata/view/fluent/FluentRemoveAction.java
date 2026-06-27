package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.RemoveAction;

/**
 * Builds {@link RemoveAction} metadata using a fluent API.
 */
public class FluentRemoveAction extends FluentAction<FluentRemoveAction> {
	private RemoveAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link RemoveAction} instance.
	 */
	public FluentRemoveAction() {
		action = new RemoveAction();
	}

	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentRemoveAction(RemoveAction action) {
		this.action = action;
	}

	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentRemoveAction from(@SuppressWarnings("hiding") RemoveAction action) {
		super.fromBase(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public RemoveAction get() {
		return action;
	}
}
