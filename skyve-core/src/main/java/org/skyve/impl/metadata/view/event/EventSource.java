package org.skyve.impl.metadata.view.event;

import org.skyve.metadata.SerializableMetaData;

/**
 * Base interface for all view widgets that can declare client-side event
 * handler lists.
 *
 * <p>Each concrete event mixin (e.g. {@link Addable}, {@link Removable},
 * {@link Selectable}) extends this interface and adds the specific handler
 * list property for its event type.
 */
public interface EventSource extends SerializableMetaData {
	public String getSource();
}
