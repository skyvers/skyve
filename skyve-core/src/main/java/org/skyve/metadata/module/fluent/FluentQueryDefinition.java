package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.QueryDefinitionMetaData;
import org.skyve.metadata.module.query.QueryDefinition;

/**
 * Builds full query definitions including descriptive and timeout fields.
 */
public abstract class FluentQueryDefinition<T extends FluentQueryDefinition<T>> extends FluentQuery<T> {
	/**
	 * Creates an empty fluent query definition wrapper.
	 */
	protected FluentQueryDefinition() {
		// nothing to see
	}

	/**
	 * Copies definition-level fields from an existing query definition.
	 *
	 * @param query The source query definition.
	 */
	@Override
	protected void from(QueryDefinition query) {
		super.from(query);
		description(query.getDescription());
		documentation(query.getDocumentation());
		timeoutInSeconds(query.getTimeoutInSeconds());
	}

	/**
	 * Sets the query description.
	 *
	 * @param description The description text.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T description(String description) {
		get().setDescription(description);
		return (T) this;
	}
	
	/**
	 * Sets the documentation text for the query.
	 *
	 * @param documentation The documentation text.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T documentation(String documentation) {
		get().setDocumentation(documentation);
		return (T) this;
	}

	/**
	 * Sets the timeout in seconds.
	 *
	 * @param timeoutInSeconds The timeout value in seconds.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T timeoutInSeconds(int timeoutInSeconds) {
		get().setTimeoutInSeconds(Integer.valueOf(timeoutInSeconds));
		return (T) this;
	}
	
	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The backing query definition metadata.
	 */
	@Override
	public abstract QueryDefinitionMetaData get();
}
