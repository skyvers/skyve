package org.skyve.metadata;

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
	 * @return the name; never {@code null}
	 */
	public String getName();
}
