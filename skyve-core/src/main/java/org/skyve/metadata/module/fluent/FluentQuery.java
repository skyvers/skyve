package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.QueryMetaData;
import org.skyve.metadata.module.query.QueryDefinition;

public abstract class FluentQuery<T extends FluentQuery<T>> {
	protected FluentQuery() {
		// nothing to see
	}

	protected void from(QueryDefinition query) {
		name(query.getName());
	}

	@SuppressWarnings("unchecked")
	public T name(String name) {
		get().setName(name);
		return (T) this;
	}

	public abstract QueryMetaData get();
}
