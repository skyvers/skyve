package org.skyve.impl.metadata.view.event;

import java.util.List;

public interface Addable extends EventSource {
	public List<EventAction> getAddedActions();
}
