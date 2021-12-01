package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.DeleteAction;

public class FluentDeleteAction extends FluentValidatableAction<FluentDeleteAction> {
	private DeleteAction action = null;
	
	public FluentDeleteAction() {
		action = new DeleteAction();
	}
	
	public FluentDeleteAction(DeleteAction action) {
		this.action = action;
	}
	
	public FluentDeleteAction from(@SuppressWarnings("hiding") DeleteAction action) {
		super.from(action);
		return this;
	}
	
	@Override
	public DeleteAction get() {
		return action;
	}
}
