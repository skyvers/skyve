package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.CustomAction;

public class FluentCustomAction extends FluentClassAction<FluentCustomAction> {
	private CustomAction action = null;
	
	public FluentCustomAction() {
		action = new CustomAction();
	}
	
	public FluentCustomAction(CustomAction action) {
		this.action = action;
	}
	
	public FluentCustomAction from(@SuppressWarnings("hiding") CustomAction action) {
		super.from(action);
		return this;
	}
	
	@Override
	public CustomAction get() {
		return action;
	}
}
