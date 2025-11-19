package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;

public abstract class FluentEventAction {
	protected FluentEventAction() {
		// nothing to see
	}

	protected static FluentEventAction from(EventAction action) {
		if (action instanceof RerenderEventAction rerender) {
			return new FluentRerenderEventAction().from(rerender);
		}
		else if (action instanceof ServerSideActionEventAction server) {
			return new FluentServerSideActionEventAction().from(server);
		}
		else if (action instanceof SetDisabledEventAction setDisabled) {
			return new FluentSetDisabledEventAction().from(setDisabled);
		}
		else if (action instanceof SetInvisibleEventAction setInvisible) {
			return new FluentSetInvisibleEventAction().from(setInvisible);
		}
		else if (action instanceof ToggleDisabledEventAction toggleDisabled) {
			return new FluentToggleDisabledEventAction().from(toggleDisabled);
		}
		else if (action instanceof ToggleVisibilityEventAction toggleVisibility) {
			return new FluentToggleVisibilityEventAction().from(toggleVisibility);
		}
		else {
			throw new IllegalArgumentException(action + " is not catered for");
		}
	}

	abstract EventAction get();
}
