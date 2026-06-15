package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.AbstractInverse;

/**
 * Provides a fluent builder for FluentInverse metadata.
 */
public abstract class FluentInverse<T extends FluentInverse<T>> extends FluentAttribute<T> {
	/**
	 * Creates a fluent builder instance.
	 */
	protected FluentInverse() {
		// nothing to see
	}
	
	/**
	 * Copies inverse relationship metadata, including cascade semantics.
	 */
	@SuppressWarnings("unchecked")
	protected T from(AbstractInverse inverse) {
		super.from(inverse);
		documentName(inverse.getDocumentName());
		referenceName(inverse.getReferenceName());
		cascade(Boolean.TRUE.equals(inverse.getCascade()));
		return (T) this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public T documentName(String documentName) {
		get().setDocumentName(documentName);
		return (T) this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public T referenceName(String referenceName) {
		get().setDocumentName(referenceName);
		return (T) this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public T cascade(boolean cascade) {
		get().setCascade(cascade ? Boolean.TRUE : Boolean.FALSE);
		return (T) this;
	}

	@Override
	public abstract AbstractInverse get();
}
