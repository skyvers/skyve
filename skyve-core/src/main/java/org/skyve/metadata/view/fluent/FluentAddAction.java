package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.AddAction;

/**
 * Builds {@link AddAction} metadata using a fluent API.
 */
public class FluentAddAction extends FluentAction<FluentAddAction> {
	private AddAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link AddAction} instance.
	 */
	public FluentAddAction() {
		action = new AddAction();
	}

	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentAddAction(AddAction action) {
		this.action = action;
	}

	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentAddAction from(@SuppressWarnings("hiding") AddAction action) {
		super.fromBase(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public AddAction get() {
		return action;
	}
}
