package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.BizExportAction;

/**
 * Builds {@link BizExportAction} metadata using a fluent API.
 */
public class FluentBizExportAction extends FluentClassAction<FluentBizExportAction> {
	private BizExportAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link BizExportAction} instance.
	 */
	public FluentBizExportAction() {
		action = new BizExportAction();
	}
	
	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentBizExportAction(BizExportAction action) {
		this.action = action;
	}
	
	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentBizExportAction from(@SuppressWarnings("hiding") BizExportAction action) {
		super.from(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public BizExportAction get() {
		return action;
	}
}
