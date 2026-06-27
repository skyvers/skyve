package org.skyve.impl.metadata.view;

import org.skyve.metadata.SerializableMetaData;

/**
 * Mixin interface for view elements that carry an optional widget identifier.
 *
 * <p>Implemented by JAXB widget and container descriptors that expose a
 * {@code widgetId} attribute, enabling programmatic targeting and DOM
 * identification in the rendered view.
 */
public interface Identifiable extends SerializableMetaData {
	String getWidgetId();
}
