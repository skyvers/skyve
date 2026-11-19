package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.CustomAction;

import jakarta.annotation.Nonnull;

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
	public FluentCustomAction(@Nonnull CustomAction action) {
		this.action = action;
	}
	
	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public @Nonnull FluentCustomAction from(@SuppressWarnings("hiding") @Nonnull CustomAction action) {
		super.from(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public @Nonnull CustomAction get() {
		return action;
	}
}
