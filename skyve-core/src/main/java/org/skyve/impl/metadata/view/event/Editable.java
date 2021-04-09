package org.skyve.impl.metadata.view.event;

import java.util.List;

public interface Editable extends EventSource {
	public List<EventAction> getEditedActions();
}
