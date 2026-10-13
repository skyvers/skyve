package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.PrintAction;

import jakarta.annotation.Nonnull;

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
	public FluentPrintAction(@Nonnull PrintAction action) {
		this.action = action;
	}
	
	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public @Nonnull FluentPrintAction from(@SuppressWarnings("hiding") @Nonnull PrintAction action) {
		super.fromBase(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public @Nonnull PrintAction get() {
		return action;
	}
}
