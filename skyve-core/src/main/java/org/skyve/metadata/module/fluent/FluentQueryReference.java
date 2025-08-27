package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.query.QueryReferenceImpl;
import org.skyve.impl.metadata.repository.module.QueryReferenceMetaData;

public abstract class FluentQueryReference<T extends FluentQueryReference<T>> extends FluentQuery<T> {
	protected FluentQueryReference() {
		// nothing to see
	}

	protected void from(QueryReferenceImpl query) {
		super.from(query);
		moduleRef(query.getModuleRef());
		ref(query.getRef());
	}

	@SuppressWarnings("unchecked")
	public T moduleRef(String moduleRef) {
		get().setModuleRef(moduleRef);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T ref(String ref) {
		get().setRef(ref);
		return (T) this;
	}

	@Override
	public abstract QueryReferenceMetaData get();
}
