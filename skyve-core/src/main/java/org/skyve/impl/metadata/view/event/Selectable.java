package org.skyve.impl.metadata.view.event;

import java.util.List;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Mixin interface for widgets that fire a row-selection event.
 *
 * <p>Implemented by grid and list widgets.  The {@code onSelectedHandlers}
 * are fired after the user selects a row.
 *
 * @see EventSource
 */
public interface Selectable extends EventSource {
	public @Nonnull List<EventAction> getSelectedActions();
	public @Nullable String getSelectedIdBinding();
	public void setSelectedIdBinding(@Nullable String selectedIdBinding);
}
