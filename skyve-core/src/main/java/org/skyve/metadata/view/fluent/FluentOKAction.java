package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.OKAction;

public class FluentOKAction extends FluentValidatableAction<FluentOKAction> {
	private OKAction action = null;
	
	public FluentOKAction() {
		action = new OKAction();
	}
	
	public FluentOKAction(OKAction action) {
		this.action = action;
	}
	
	public FluentOKAction from(@SuppressWarnings("hiding") OKAction action) {
		super.fromBase(action);
		return this;
	}
	
	@Override
	public OKAction get() {
		return action;
	}
}
