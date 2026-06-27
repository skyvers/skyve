package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.PrintAction;

/**
 * Builds {@link PrintAction} metadata using a fluent API.
 */
public class FluentPrintAction extends FluentValidatableAction<FluentPrintAction> {
	private PrintAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link PrintAction} instance.
	 */
	public FluentPrintAction() {
		action = new PrintAction();
	}
	
	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentPrintAction(PrintAction action) {
		this.action = action;
	}
	
	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentPrintAction from(@SuppressWarnings("hiding") PrintAction action) {
		super.fromBase(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public PrintAction get() {
		return action;
	}
}
