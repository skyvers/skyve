package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.RemoveAction;

public class FluentRemoveAction extends FluentAction<FluentRemoveAction> {
	private RemoveAction action = null;
	
	public FluentRemoveAction() {
		action = new RemoveAction();
	}

	public FluentRemoveAction(RemoveAction action) {
		this.action = action;
	}

	public FluentRemoveAction from(@SuppressWarnings("hiding") RemoveAction action) {
		super.fromBase(action);
		return this;
	}
	
	@Override
	public RemoveAction get() {
		return action;
	}
}
