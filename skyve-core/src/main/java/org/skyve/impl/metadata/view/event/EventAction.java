package org.skyve.impl.metadata.view.event;

import org.skyve.metadata.DecoratedMetaData;

/**
 * Marker interface for all client-side event-response actions defined in view
 * metadata.
 *
 * <p>Implementations include rerender, server-side action invocation,
 * set-disabled, set-invisible, toggle-disabled, and toggle-visibility
 * responses that are attached to widget event handlers.
 *
 * @see EventSource
 */
public interface EventAction extends DecoratedMetaData {
	// nothing to see here
}
