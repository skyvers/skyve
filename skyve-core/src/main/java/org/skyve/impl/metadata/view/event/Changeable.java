package org.skyve.impl.metadata.view.event;

import java.util.List;

import org.skyve.metadata.view.widget.bound.Bound;

public interface Changeable extends Bound {
	public List<EventAction> getChangedActions();
}
