package org.skyve.impl.metadata.view.event;

import java.util.List;

import jakarta.annotation.Nonnull;

/**
 * Mixin interface for widgets that fire an inline-edit event.
 *
 * <p>Implemented by grid widgets that support inline cell editing.  The
 * {@code onEditedHandlers} are fired after the user commits an inline edit.
 *
 * @see EventSource
 */
public interface Editable extends EventSource {
	public @Nonnull List<EventAction> getEditedActions();
}
