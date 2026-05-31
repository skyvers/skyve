package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.query.QueryReferenceImpl;
import org.skyve.impl.metadata.repository.module.QueryReferenceMetaData;

/**
 * Builds query reference metadata that points to definitions in this or another module.
 */
public abstract class FluentQueryReference<T extends FluentQueryReference<T>> extends FluentQuery<T> {
	/**
	 * Creates an empty fluent query reference wrapper.
	 */
	protected FluentQueryReference() {
		// nothing to see
	}

	/**
	 * Copies common reference fields from an existing query reference.
	 *
	 * @param query The source query reference.
	 */
	protected void from(QueryReferenceImpl query) {
		super.from(query);
		moduleRef(query.getModuleRef());
		ref(query.getRef());
	}

	/**
	 * Sets the module reference for cross-module queries.
	 *
	 * @param moduleRef The referenced module name.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T moduleRef(String moduleRef) {
		get().setModuleRef(moduleRef);
		return (T) this;
	}

	/**
	 * Sets the referenced query name.
	 *
	 * @param ref The referenced query identifier.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T ref(String ref) {
		get().setRef(ref);
		return (T) this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The backing query reference metadata.
	 */
	@Override
	public abstract QueryReferenceMetaData get();
}
