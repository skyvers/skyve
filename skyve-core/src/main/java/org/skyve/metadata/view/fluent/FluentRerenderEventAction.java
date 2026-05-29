package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.RerenderEventAction;

/**
 * Builds {@link RerenderEventAction} metadata using a fluent API.
 */
public class FluentRerenderEventAction extends FluentEventAction {
	private RerenderEventAction action = null;

	/**
	 * Creates a fluent builder backed by a new {@link RerenderEventAction} instance.
	 */
	public FluentRerenderEventAction() {
		action = new RerenderEventAction();
	}

	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentRerenderEventAction(RerenderEventAction action) {
		this.action = action;
	}

	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentRerenderEventAction from(@SuppressWarnings("hiding") RerenderEventAction action) {
		clientValidation(! Boolean.FALSE.equals(action.getClientValidation()));
		return this;
	}

	/**
	 * Sets whether client-side validation is required before rerender.
	 *
	 * @param clientValidation
	 *            whether client validation is required
	 * @return this builder
	 */
	public FluentRerenderEventAction clientValidation(boolean clientValidation) {
		action.setClientValidation(clientValidation ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Returns the wrapped event action instance.
	 */
	@Override
	public RerenderEventAction get() {
		return action;
	}
}
