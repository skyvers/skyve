package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.QueryMetaData;
import org.skyve.metadata.module.query.QueryDefinition;

import jakarta.annotation.Nonnull;

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
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T putProperty(@Nonnull String k, @Nonnull String v) {
		get().getProperties().put(k, v);
		return (T) this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The backing metadata instance.
	 */
	public abstract QueryMetaData get();
}
