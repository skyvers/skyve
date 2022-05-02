package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.DefaultsAction;

public class FluentDefaultsAction extends FluentAction<FluentDefaultsAction> {
	private DefaultsAction action = null;
	
	public FluentDefaultsAction() {
		action = new DefaultsAction();
	}

	public FluentDefaultsAction(DefaultsAction action) {
		this.action = action;
	}

	public FluentDefaultsAction from(@SuppressWarnings("hiding") DefaultsAction action) {
		super.fromBase(action);
		return this;
	}
	
	@Override
	public DefaultsAction get() {
		return action;
	}
}
