package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.query.MetaDataQueryReferenceImpl;
import org.skyve.impl.metadata.repository.module.MetaDataQueryReferenceMetaData;

import jakarta.annotation.Nonnull;

/**
 * Builds metadata query references.
 */
public class FluentMetaDataQueryReference extends FluentQueryReference<FluentMetaDataQueryReference> {
	private MetaDataQueryReferenceMetaData query = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentMetaDataQueryReference() {
		query = new MetaDataQueryReferenceMetaData();
	}
	
	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param query The metadata to mutate.
	 */
	public FluentMetaDataQueryReference(@Nonnull MetaDataQueryReferenceMetaData query) {
		this.query = query;
	}
	
	/**
	 * Copies the reference fields from an existing metadata query reference.
	 *
	 * @param query The source reference.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentMetaDataQueryReference from(@SuppressWarnings("hiding") @Nonnull MetaDataQueryReferenceImpl query) {
		super.from(query);
		return this;
	}
	
	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The metadata query reference metadata instance.
	 */
	@Override
	public @Nonnull MetaDataQueryReferenceMetaData get() {
		return query;
	}
}
