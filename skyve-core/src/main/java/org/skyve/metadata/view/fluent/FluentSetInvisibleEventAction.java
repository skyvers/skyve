package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;

public class FluentSetInvisibleEventAction extends FluentEventAction {
	private SetInvisibleEventAction action = null;

	public FluentSetInvisibleEventAction() {
		action = new SetInvisibleEventAction();
	}

	public FluentSetInvisibleEventAction(SetInvisibleEventAction action) {
		this.action = action;
	}

	public FluentSetInvisibleEventAction from(@SuppressWarnings("hiding") SetInvisibleEventAction action) {
		binding(action.getBinding());
		invisibleConditionName(action.getInvisibleConditionName());
		return this;
	}

	public FluentSetInvisibleEventAction binding(String binding) {
		action.setBinding(binding);
		return this;
	}

	public FluentSetInvisibleEventAction invisibleConditionName(String invisibleConditionName) {
		action.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	@Override
	public SetInvisibleEventAction get() {
		return action;
	}
}
