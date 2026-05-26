package org.skyve.impl.metadata.view.event;

import java.util.List;

/**
 * Mixin interface for input widgets that fire focus and blur events.
 *
 * <p>Implemented by widgets such as text fields and combos that can receive
 * keyboard focus.  The {@code onFocusHandlers} and {@code onBlurHandlers}
 * lists are fired when the widget gains or loses focus.
 *
 * @see EventSource
 */
public interface Focusable extends EventSource {
	public List<EventAction> getFocusActions();
	public List<EventAction> getBlurActions();
}
