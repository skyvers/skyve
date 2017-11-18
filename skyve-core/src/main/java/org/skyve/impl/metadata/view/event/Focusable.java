package org.skyve.impl.metadata.view.event;

import java.util.List;

import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.EventSource;

public interface Focusable extends EventSource {
	public List<EventAction> getFocusActions();
	public List<EventAction> getBlurActions();
}
