package org.skyve.impl.metadata.view.event;

import java.util.List;

import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.impl.metadata.view.event.EventAction;

public interface Changeable extends Bound {
	public List<EventAction> getChangedActions();
}
