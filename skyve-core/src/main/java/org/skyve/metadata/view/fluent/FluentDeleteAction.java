package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.DeleteAction;

/**
 * Builds {@link DeleteAction} metadata using a fluent API.
 */
public class FluentDeleteAction extends FluentValidatableAction<FluentDeleteAction> {
	private DeleteAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link DeleteAction} instance.
	 */
	public FluentDeleteAction() {
		action = new DeleteAction();
	}
	
	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentDeleteAction(DeleteAction action) {
		this.action = action;
	}
	
	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentDeleteAction from(@SuppressWarnings("hiding") DeleteAction action) {
		super.fromBase(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public DeleteAction get() {
		return action;
	}
}
