package org.skyve.metadata;

import jakarta.annotation.Nullable;

/**
 * Metadata that carries an identifier name.
 *
 * <p>Most Skyve metadata objects (modules, documents, attributes, queries, roles,
 * menus, etc.) have a name that uniquely identifies them within their containing
 * scope. This interface provides a uniform accessor for that name.
 *
 * @see SerializableMetaData
 */
public interface NamedMetaData extends SerializableMetaData {
	/**
	 * Returns the identifier name of this metadata element.
	 *
	 * <p>The name is unique within its containing scope (e.g. unique among
	 * attributes of a document, or among documents of a module).
	 *
	 * <p>Sub-interfaces whose name is always set once the metadata is loaded redeclare
	 * this method as non-null. A {@link org.skyve.metadata.model.Persistent} name is
	 * {@code null} when the document is not mapped to a table.
	 *
	 * @return the name, or {@code null} where the metadata has none
	 */
	public @Nullable String getName();
}
