package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.QueryMetaData;
import org.skyve.metadata.module.query.QueryDefinition;

/**
 * Builds query metadata with fluent setters shared by query definitions and references.
 */
public abstract class FluentQuery<T extends FluentQuery<T>> {
	/**
	 * Creates an empty fluent query wrapper.
	 */
	protected FluentQuery() {
		// nothing to see
	}

	/**
	 * Copies common query fields from an existing definition.
	 *
	 * @param query The source query definition to copy from.
	 */
	protected void from(QueryDefinition query) {
		name(query.getName());
	}

	/**
	 * Sets the query name.
	 *
	 * @param name The query name.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T name(String name) {
		get().setName(name);
		return (T) this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The backing metadata instance.
	 */
	public abstract QueryMetaData get();
}
