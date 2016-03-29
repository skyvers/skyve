package org.skyve.wildcat.metadata.view.event;

import java.util.List;

public interface Removable extends EventSource {
	public List<EventAction> getRemovedActions();
}
