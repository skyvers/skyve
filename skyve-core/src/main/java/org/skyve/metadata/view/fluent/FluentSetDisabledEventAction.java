package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.SetDisabledEventAction;

public class FluentSetDisabledEventAction extends FluentEventAction {
	private SetDisabledEventAction action = null;

	public FluentSetDisabledEventAction() {
		action = new SetDisabledEventAction();
	}

	public FluentSetDisabledEventAction(SetDisabledEventAction action) {
		this.action = action;
	}

	public FluentSetDisabledEventAction from(@SuppressWarnings("hiding") SetDisabledEventAction action) {
		binding(action.getBinding());
		disabledConditionName(action.getDisabledConditionName());

		return this;
	}

	public FluentSetDisabledEventAction binding(String binding) {
		action.setBinding(binding);
		return this;
	}

	public FluentSetDisabledEventAction disabledConditionName(String disabledConditionName) {
		action.setDisabledConditionName(disabledConditionName);
		return this;
	}

	@Override
	public SetDisabledEventAction get() {
		return action;
	}
}
