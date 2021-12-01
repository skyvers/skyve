package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.AddAction;

public class FluentAddAction extends FluentAction<FluentAddAction> {
	private AddAction action = null;
	
	public FluentAddAction() {
		action = new AddAction();
	}

	public FluentAddAction(AddAction action) {
		this.action = action;
	}

	public FluentAddAction from(@SuppressWarnings("hiding") AddAction action) {
		super.from(action);
		return this;
	}
	
	@Override
	public AddAction get() {
		return action;
	}
}
