package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.ReferenceImpl;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Reference;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides a fluent builder for FluentReference metadata.
 */
public abstract class FluentReference<T extends FluentReference<T>> extends FluentAttribute<T> {
	/**
	 * Creates a fluent builder instance.
	 */
	protected FluentReference() {
		// nothing to see
	}

	/**
	 * Copies reference metadata, including target document/query configuration.
	 */
	@SuppressWarnings("unchecked")
	protected @Nonnull T from(@Nonnull Reference reference) {
		super.from(reference);
		persistent(reference.isPersistent());
		domainType(reference.getDomainType());
		documentName(reference.getDocumentName());
		queryName(reference.getQueryName());
		return (T) this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T persistent(boolean persistent) {
		get().setPersistent(persistent);
		return (T) this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T domainType(@Nullable DomainType domainType) {
		get().setDomainType(domainType);
		return (T) this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T documentName(@Nullable String documentName) {
		get().setDocumentName(documentName);
		return (T) this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T queryName(@Nullable String queryName) {
		get().setQueryName(queryName);
		return (T) this;
	}

	@Override
	public abstract @Nonnull ReferenceImpl get();
}
