package org.skyve.impl.metadata.view.event;

import java.util.List;

/**
 * Mixin interface for widget containers that fire an add-item event.
 *
 * <p>Implemented by list/grid widgets that allow the user to add a new row.
 * The {@code onAddedHandlers} declared on the widget are fired after the add.
 *
 * @see EventSource
 */
public interface Addable extends EventSource {
	public List<EventAction> getAddedActions();
}
