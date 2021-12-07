package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;

public class FluentToggleDisabledEventAction {
	private ToggleDisabledEventAction action = null;

	public FluentToggleDisabledEventAction() {
		action = new ToggleDisabledEventAction();
	}

	public FluentToggleDisabledEventAction(ToggleDisabledEventAction action) {
		this.action = action;
	}

	public FluentToggleDisabledEventAction from(@SuppressWarnings("hiding") ToggleDisabledEventAction action) {
		binding(action.getBinding());
		return this;
	}

	public FluentToggleDisabledEventAction binding(String binding) {
		action.setBinding(binding);
		return this;
	}

	public ToggleDisabledEventAction get() {
		return action;
	}
}
