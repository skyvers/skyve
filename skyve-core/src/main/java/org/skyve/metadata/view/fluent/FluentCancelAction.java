package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.CancelAction;

public class FluentCancelAction extends FluentAction<FluentCancelAction> {
	private CancelAction action = null;
	
	public FluentCancelAction() {
		action = new CancelAction();
	}

	public FluentCancelAction(CancelAction action) {
		this.action = action;
	}

	public FluentCancelAction from(@SuppressWarnings("hiding") CancelAction action) {
		super.from(action);
		return this;
	}
	
	@Override
	public CancelAction get() {
		return action;
	}
}
