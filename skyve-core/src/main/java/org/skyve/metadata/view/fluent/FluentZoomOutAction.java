package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.ZoomOutAction;

/**
 * Builds {@link ZoomOutAction} metadata using a fluent API.
 */
public class FluentZoomOutAction extends FluentValidatableAction<FluentZoomOutAction> {
	private ZoomOutAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link ZoomOutAction} instance.
	 */
	public FluentZoomOutAction() {
		action = new ZoomOutAction();
	}
	
	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentZoomOutAction(ZoomOutAction action) {
		this.action = action;
	}
	
	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public FluentZoomOutAction from(@SuppressWarnings("hiding") ZoomOutAction action) {
		super.from(action);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public ZoomOutAction get() {
		return action;
	}
}
