package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.EventAction;

abstract class FluentEventAction {
	protected FluentEventAction() {
		// nothing to see
	}

	protected static FluentEventAction from(EventAction action) {
/* TODO uncomment this if
		if (action instanceof RerenderEventAction) {
			new FluentRerenderEventAction().from((RerenderEventAction) action);
		}
		else if (action instanceof ServerSideActionEventAction) {
			new FluentServerSideActionEventAction().from((ServerSideActionEventAction) action);
		}
		else if (action instanceof SetDisabledEventAction) {
			new SetDisabledEventAction().from((SetDisabledEventAction) action);
		}
		else if (action instanceof SetInvisibleEventAction) {
			new SetInvisibleEventAction().from((SetInvisibleEventAction) action);
		}
		else if (action instanceof ToggleDisabledEventAction) {
			new ToggleDisabledEventAction().from((ToggleDisabledEventAction) action);
		}
		else if (action instanceof ToggleVisibilityEventAction) {
			new ToggleVisibilityEventAction().from((ToggleVisibilityEventAction) action);
		}
		else {
			throw new IllegalArgumentException(action + " is not catered for");
		}
and TODO implement the below classes
RerenderEventAction
ServerSideActionEventAction
SetDisabledEventAction
SetInvisibleEventAction
ToggleDisabledEventAction
ToggleVisibilityEventAction
*/

		return null;
	}
	
	abstract EventAction get();
}
