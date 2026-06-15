package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.OKAction;

/**
 * Builds {@link OKAction} metadata using a fluent API.
 */
public class FluentOKAction extends FluentValidatableAction<FluentOKAction> {
	private OKAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link OKAction} instance.
	 */
	public FluentOKAction() {
		action = new OKAction();
	}
	
	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentOKAction(OKAction action) {
		this.action = action;
	}
	
	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentOKAction from(@SuppressWarnings("hiding") OKAction action) {
		super.fromBase(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public OKAction get() {
		return action;
	}
}
