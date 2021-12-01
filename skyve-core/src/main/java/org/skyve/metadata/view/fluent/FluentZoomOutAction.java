package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.ZoomOutAction;

public class FluentZoomOutAction extends FluentValidatableAction<FluentZoomOutAction> {
	private ZoomOutAction action = null;
	
	public FluentZoomOutAction() {
		action = new ZoomOutAction();
	}
	
	public FluentZoomOutAction(ZoomOutAction action) {
		this.action = action;
	}
	
	public FluentZoomOutAction from(@SuppressWarnings("hiding") ZoomOutAction action) {
		super.from(action);
		return this;
	}
	
	@Override
	public ZoomOutAction get() {
		return action;
	}
}
