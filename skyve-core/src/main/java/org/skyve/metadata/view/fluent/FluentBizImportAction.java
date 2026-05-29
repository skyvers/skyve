package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.BizImportAction;

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
	public FluentBizImportAction(BizImportAction action) {
		this.action = action;
	}
	
	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentBizImportAction from(@SuppressWarnings("hiding") BizImportAction action) {
		super.from(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public BizImportAction get() {
		return action;
	}
}
