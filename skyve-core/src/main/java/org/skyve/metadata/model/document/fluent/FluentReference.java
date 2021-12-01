package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.ReferenceImpl;
import org.skyve.metadata.model.document.Reference;

public abstract class FluentReference<T extends FluentReference<T>> extends FluentAttribute<T> {
	protected FluentReference() {
		// nothing to see
	}

	@SuppressWarnings("unchecked")
	protected T from(Reference reference) {
		super.from(reference);
		persistent(reference.isPersistent());
		documentName(reference.getDocumentName());
		queryName(reference.getQueryName());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T persistent(boolean persistent) {
		get().setPersistent(persistent);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T documentName(String documentName) {
		get().setDocumentName(documentName);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T queryName(String queryName) {
		get().setQueryName(queryName);
		return (T) this;
	}

	@Override
	public abstract ReferenceImpl get();
}
