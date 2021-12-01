package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.NewAction;

public class FluentNewAction extends FluentAction<FluentNewAction> {
	private NewAction action = null;
	
	public FluentNewAction() {
		action = new NewAction();
	}

	public FluentNewAction(NewAction action) {
		this.action = action;
	}

	public FluentNewAction from(@SuppressWarnings("hiding") NewAction action) {
		super.from(action);
		return this;
	}
	
	@Override
	public NewAction get() {
		return action;
	}
}
