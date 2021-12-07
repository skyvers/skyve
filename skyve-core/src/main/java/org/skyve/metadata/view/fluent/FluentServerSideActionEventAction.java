package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;

public class FluentServerSideActionEventAction {
	private ServerSideActionEventAction action = null;

	public FluentServerSideActionEventAction() {
		action = new ServerSideActionEventAction();
	}

	public FluentServerSideActionEventAction(ServerSideActionEventAction action) {
		this.action = action;
	}

	public FluentServerSideActionEventAction from(@SuppressWarnings("hiding") ServerSideActionEventAction action) {
		actionName(action.getActionName());
		return this;
	}

	public FluentServerSideActionEventAction actionName(String actionName) {
		action.setActionName(actionName);
		return this;
	}

	public ServerSideActionEventAction get() {
		return action;
	}
}
