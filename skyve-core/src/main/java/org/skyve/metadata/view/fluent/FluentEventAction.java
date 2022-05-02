package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;

abstract class FluentEventAction {
	protected FluentEventAction() {
		// nothing to see
	}

	protected static FluentEventAction from(EventAction action) {
		if (action instanceof RerenderEventAction) {
			return new FluentRerenderEventAction().from((RerenderEventAction) action);
		}
		else if (action instanceof ServerSideActionEventAction) {
			return new FluentServerSideActionEventAction().from((ServerSideActionEventAction) action);
		}
		else if (action instanceof SetDisabledEventAction) {
			return new FluentSetDisabledEventAction().from((SetDisabledEventAction) action);
		}
		else if (action instanceof SetInvisibleEventAction) {
			return new FluentSetInvisibleEventAction().from((SetInvisibleEventAction) action);
		}
		else if (action instanceof ToggleDisabledEventAction) {
			return new FluentToggleDisabledEventAction().from((ToggleDisabledEventAction) action);
		}
		else if (action instanceof ToggleVisibilityEventAction) {
			return new FluentToggleVisibilityEventAction().from((ToggleVisibilityEventAction) action);
		}
		else {
			throw new IllegalArgumentException(action + " is not catered for");
		}
	}

	abstract EventAction get();
}
