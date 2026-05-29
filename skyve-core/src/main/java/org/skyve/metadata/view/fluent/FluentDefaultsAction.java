package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.DefaultsAction;

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
	public FluentDefaultsAction(DefaultsAction action) {
		this.action = action;
	}

	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentDefaultsAction from(@SuppressWarnings("hiding") DefaultsAction action) {
		super.fromBase(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public DefaultsAction get() {
		return action;
	}
}
