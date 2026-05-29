package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.CustomAction;

/**
 * Builds {@link CustomAction} metadata using a fluent API.
 */
public class FluentCustomAction extends FluentClassAction<FluentCustomAction> {
	private CustomAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link CustomAction} instance.
	 */
	public FluentCustomAction() {
		action = new CustomAction();
	}
	
	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentCustomAction(CustomAction action) {
		this.action = action;
	}
	
	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentCustomAction from(@SuppressWarnings("hiding") CustomAction action) {
		super.from(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public CustomAction get() {
		return action;
	}
}
