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
	 * Returns the custom properties associated with this metadata element.
	 *
	 * <p>The returned map is the live backing map, not a copy. It is populated while
	 * metadata is loaded, converted or built through the fluent API; consumers of
	 * loaded metadata must treat it as read-only.
	 *
	 * <p>Iteration order is the order in which the properties were declared in XML or
	 * added through the fluent API; replacing a property keeps its original position.
	 *
	 * @return a map of property name to value; never {@code null}, may be empty
	 */
	public Map<String, String> getProperties();
}
