package org.skyve.impl.metadata.view.model;

import org.skyve.metadata.SerializableMetaData;

/**
 * Marker interface for view model descriptors referenced by model-driven
 * widgets such as charts and maps.
 *
 * <p>Implementations provide a stable model name used by widgets to resolve
 * the model definition at runtime.
 */
public interface ModelMetaData extends SerializableMetaData {
	public String getModelName();
}
