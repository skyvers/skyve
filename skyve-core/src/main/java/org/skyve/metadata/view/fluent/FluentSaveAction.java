package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.SaveAction;

public class FluentSaveAction extends FluentValidatableAction<FluentSaveAction> {
	private SaveAction action = null;
	
	public FluentSaveAction() {
		action = new SaveAction();
	}
	
	public FluentSaveAction(SaveAction action) {
		this.action = action;
	}
	
	public FluentSaveAction from(@SuppressWarnings("hiding") SaveAction action) {
		super.from(action);
		return this;
	}
	
	@Override
	public SaveAction get() {
		return action;
	}
}
