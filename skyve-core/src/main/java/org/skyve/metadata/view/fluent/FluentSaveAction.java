package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.SaveAction;

import jakarta.annotation.Nonnull;

/**
 * Builds {@link SaveAction} metadata using a fluent API.
 */
public class FluentSaveAction extends FluentValidatableAction<FluentSaveAction> {
	private SaveAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link SaveAction} instance.
	 */
	public FluentSaveAction() {
		action = new SaveAction();
	}
	
	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentSaveAction(@Nonnull SaveAction action) {
		this.action = action;
	}
	
	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public @Nonnull FluentSaveAction from(@SuppressWarnings("hiding") @Nonnull SaveAction action) {
		super.from(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public @Nonnull SaveAction get() {
		return action;
	}
}
