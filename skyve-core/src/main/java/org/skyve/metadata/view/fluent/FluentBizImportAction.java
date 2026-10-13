package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.BizImportAction;

import jakarta.annotation.Nonnull;

/**
 * Builds {@link BizImportAction} metadata using a fluent API.
 */
public class FluentBizImportAction extends FluentClassAction<FluentBizImportAction> {
	private BizImportAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link BizImportAction} instance.
	 */
	public FluentBizImportAction() {
		action = new BizImportAction();
	}
	
	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentBizImportAction(@Nonnull BizImportAction action) {
		this.action = action;
	}
	
	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public @Nonnull FluentBizImportAction from(@SuppressWarnings("hiding") @Nonnull BizImportAction action) {
		super.from(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public @Nonnull BizImportAction get() {
		return action;
	}
}
