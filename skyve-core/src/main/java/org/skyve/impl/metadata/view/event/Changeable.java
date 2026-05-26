package org.skyve.impl.metadata.view.event;

import java.util.List;

import org.skyve.metadata.view.widget.bound.Bound;

/**
 * Mixin interface for bound input widgets that fire a value-change event.
 *
 * <p>Implemented by widgets whose bound attribute value can change in the UI.
 * The {@code onChangedHandlers} are fired whenever the bound value is modified
 * by the user or a client-side action.
 *
 * @see org.skyve.metadata.view.widget.bound.Bound
 */
public interface Changeable extends Bound {
	public List<EventAction> getChangedActions();
}
