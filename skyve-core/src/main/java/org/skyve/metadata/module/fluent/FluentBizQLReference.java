package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.query.BizQLReferenceImpl;
import org.skyve.impl.metadata.repository.module.BizQLReferenceMetaData;

/**
 * Builds BizQL query references.
 */
public class FluentBizQLReference extends FluentQueryReference<FluentBizQLReference> {
	private BizQLReferenceMetaData bizql = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentBizQLReference() {
		bizql = new BizQLReferenceMetaData();
	}
	
	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param bizql The metadata to mutate.
	 */
	public FluentBizQLReference(BizQLReferenceMetaData bizql) {
		this.bizql = bizql;
	}
	
	/**
	 * Copies the reference fields from an existing BizQL reference.
	 *
	 * @param bizql The source reference.
	 * @return this fluent instance.
	 */
	public FluentBizQLReference from(@SuppressWarnings("hiding") BizQLReferenceImpl bizql) {
		super.from(bizql);
		return this;
	}
	
	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The BizQL reference metadata instance.
	 */
	@Override
	public BizQLReferenceMetaData get() {
		return bizql;
	}
}
