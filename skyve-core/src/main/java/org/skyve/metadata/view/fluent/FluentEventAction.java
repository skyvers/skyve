package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.event.EventAction;

abstract class FluentEventAction {
	protected FluentEventAction() {
		// nothing to see
	}

	protected static FluentEventAction from(EventAction action) {
/* TODO implement the below classes
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
