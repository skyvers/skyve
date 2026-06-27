package org.skyve.impl.metadata.view.event;

import java.util.List;

/**
 * Mixin interface for widget containers that fire a remove-item event.
 *
 * <p>Implemented by list/grid widgets that allow the user to remove a row.
 * The {@code onRemovedHandlers} are fired after the row is removed.
 *
 * @see EventSource
 */
public interface Removable extends EventSource {
	public List<EventAction> getRemovedActions();
}
