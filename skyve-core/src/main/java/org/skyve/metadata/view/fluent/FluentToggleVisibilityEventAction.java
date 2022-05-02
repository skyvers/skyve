package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;

public class FluentToggleVisibilityEventAction extends FluentEventAction {
	private ToggleVisibilityEventAction action = null;

	public FluentToggleVisibilityEventAction() {
		action = new ToggleVisibilityEventAction();
	}

	public FluentToggleVisibilityEventAction(ToggleVisibilityEventAction action) {
		this.action = action;
	}

	public FluentToggleVisibilityEventAction from(@SuppressWarnings("hiding") ToggleVisibilityEventAction action) {
		binding(action.getBinding());
		return this;
	}

	public FluentToggleVisibilityEventAction binding(String binding) {
		action.setBinding(binding);
		return this;
	}

	@Override
	public ToggleVisibilityEventAction get() {
		return action;
	}
}
