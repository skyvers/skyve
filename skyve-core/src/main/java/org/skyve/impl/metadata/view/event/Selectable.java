package org.skyve.impl.metadata.view.event;

import java.util.List;

/**
 * Mixin interface for widgets that fire a row-selection event.
 *
 * <p>Implemented by grid and list widgets.  The {@code onSelectedHandlers}
 * are fired after the user selects a row.
 *
 * @see EventSource
 */
public interface Selectable extends EventSource {
	public List<EventAction> getSelectedActions();
	public String getSelectedIdBinding();
	public void setSelectedIdBinding(String selectedIdBinding);
}
