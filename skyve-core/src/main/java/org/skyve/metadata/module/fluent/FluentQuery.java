package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.QueryMetaData;
import org.skyve.metadata.module.query.QueryDefinition;

abstract class FluentQuery<T extends FluentQuery<T>> {
	protected FluentQuery() {
		// nothing to see
	}

	protected void from(QueryDefinition query) {
		name(query.getName());
		description(query.getDescription());
		documentation(query.getDocumentation());
		timeoutInSeconds(query.getTimeoutInSeconds());
	}
	
	@SuppressWarnings("unchecked")
	public T name(String name) {
		get().setName(name);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T description(String description) {
		get().setDescription(description);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T documentation(String documentation) {
		get().setDocumentation(documentation);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T timeoutInSeconds(int timeoutInSeconds) {
		get().setTimeoutInSeconds(Integer.valueOf(timeoutInSeconds));
		return (T) this;
	}

	public abstract QueryMetaData get();
}
