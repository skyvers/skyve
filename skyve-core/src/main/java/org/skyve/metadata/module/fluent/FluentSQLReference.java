package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.query.SQLReferenceImpl;
import org.skyve.impl.metadata.repository.module.SQLReferenceMetaData;

import jakarta.annotation.Nonnull;

/**
 * Builds SQL query references.
 */
public class FluentSQLReference extends FluentQueryReference<FluentSQLReference> {
	private SQLReferenceMetaData sql = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentSQLReference() {
		this.sql = new SQLReferenceMetaData();
	}
	
	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param sql The metadata to mutate.
	 */
	public FluentSQLReference(@Nonnull SQLReferenceMetaData sql) {
		this.sql = sql;
	}

	/**
	 * Copies the reference fields from an existing SQL reference.
	 *
	 * @param sql The source reference.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentSQLReference from(@SuppressWarnings("hiding") @Nonnull SQLReferenceImpl sql) {
		super.from(sql);
		return this;
	}
	
	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The SQL reference metadata instance.
	 */
	@Override
	public @Nonnull SQLReferenceMetaData get() {
		return sql;
	}
}
