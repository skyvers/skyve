package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.AbstractInverse;

abstract class FluentInverse<T extends FluentInverse<T>> extends FluentAttribute<T> {
	protected FluentInverse() {
		// nothing to see
	}
	
	@SuppressWarnings("unchecked")
	protected T from(AbstractInverse inverse) {
		super.from(inverse);
		documentName(inverse.getDocumentName());
		referenceName(inverse.getReferenceName());
		cascade(Boolean.TRUE.equals(inverse.getCascade()));
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T documentName(String documentName) {
		get().setDocumentName(documentName);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T referenceName(String referenceName) {
		get().setDocumentName(referenceName);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T cascade(boolean cascade) {
		get().setCascade(cascade ? Boolean.TRUE : Boolean.FALSE);
		return (T) this;
	}

	@Override
	public abstract AbstractInverse get();
}
