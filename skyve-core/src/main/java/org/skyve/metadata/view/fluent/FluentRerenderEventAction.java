package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.RerenderEventAction;

public class FluentRerenderEventAction extends FluentEventAction {
	private RerenderEventAction action = null;

	public FluentRerenderEventAction() {
		action = new RerenderEventAction();
	}

	public FluentRerenderEventAction(RerenderEventAction action) {
		this.action = action;
	}

	public FluentRerenderEventAction from(@SuppressWarnings("hiding") RerenderEventAction action) {
		clientValidation(! Boolean.FALSE.equals(action.getClientValidation()));
		return this;
	}

	public FluentRerenderEventAction clientValidation(boolean clientValidation) {
		action.setClientValidation(clientValidation ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	@Override
	public RerenderEventAction get() {
		return action;
	}
}
