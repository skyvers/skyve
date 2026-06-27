package org.skyve.metadata;

import java.util.Map;

/**
 * Metadata that supports arbitrary key-value property extensions.
 *
 * <p>Custom properties allow metadata consumers (customer overrides, plugins,
 * tooling) to attach implementation-specific configuration to any metadata
 * element without extending the schema. Properties are declared in the
 * {@code <properties>} section of the relevant XML file.
 *
 * @see SerializableMetaData
 */
public interface DecoratedMetaData extends SerializableMetaData {
	/**
	 * Returns a read-only map of custom properties associated with this metadata element.
	 *
	 * @return a map of property name to value; never {@code null}, may be empty
	 */
	public Map<String, String> getProperties();
}
