package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.QueryDefinitionMetaData;
import org.skyve.metadata.module.query.QueryDefinition;

public abstract class FluentQueryDefinition<T extends FluentQueryDefinition<T>> extends FluentQuery<T> {
	protected FluentQueryDefinition() {
		// nothing to see
	}

	@Override
	protected void from(QueryDefinition query) {
		super.from(query);
		description(query.getDescription());
		documentation(query.getDocumentation());
		timeoutInSeconds(query.getTimeoutInSeconds());
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
	
	@Override
	public abstract QueryDefinitionMetaData get();
}
