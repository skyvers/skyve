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
			new FluentRerenderEventAction().from((RerenderEventAction) action);
		} else if (action instanceof ServerSideActionEventAction) {
			new FluentServerSideActionEventAction().from((ServerSideActionEventAction) action);
		} else if (action instanceof SetDisabledEventAction) {
			new FluentSetDisabledEventAction().from((SetDisabledEventAction) action);
		} else if (action instanceof SetInvisibleEventAction) {
			new FluentSetInvisibleEventAction().from((SetInvisibleEventAction) action);
		} else if (action instanceof ToggleDisabledEventAction) {
			new FluentToggleDisabledEventAction().from((ToggleDisabledEventAction) action);
		} else if (action instanceof ToggleVisibilityEventAction) {
			new FluentToggleVisibilityEventAction().from((ToggleVisibilityEventAction) action);
		} else {
			throw new IllegalArgumentException(action + " is not catered for");
		}

		return null;
	}

	abstract EventAction get();
}
