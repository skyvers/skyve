package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.query.MetaDataQueryReferenceImpl;
import org.skyve.impl.metadata.repository.module.MetaDataQueryReferenceMetaData;

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
	public FluentMetaDataQueryReference(MetaDataQueryReferenceMetaData query) {
		this.query = query;
	}
	
	/**
	 * Copies the reference fields from an existing metadata query reference.
	 *
	 * @param query The source reference.
	 * @return this fluent instance.
	 */
	public FluentMetaDataQueryReference from(@SuppressWarnings("hiding") MetaDataQueryReferenceImpl query) {
		super.from(query);
		return this;
	}
	
	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The metadata query reference metadata instance.
	 */
	@Override
	public MetaDataQueryReferenceMetaData get() {
		return query;
	}
}
