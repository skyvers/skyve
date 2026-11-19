package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.SQLMetaData;
import org.skyve.metadata.module.query.SQLDefinition;

import jakarta.annotation.Nonnull;

/**
 * Builds {@link SQLMetaData} query definitions.
 */
public class FluentSQL extends FluentQueryDefinition<FluentSQL> {
	private SQLMetaData sql = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentSQL() {
		this.sql = new SQLMetaData();
	}
	
	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param sql The metadata to mutate.
	 */
	public FluentSQL(@Nonnull SQLMetaData sql) {
		this.sql = sql;
	}

	/**
	 * Copies all definition fields from an existing SQL definition.
	 *
	 * @param sql The source definition.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentSQL from(@SuppressWarnings("hiding") @Nonnull SQLDefinition sql) {
		super.from(sql);
		query(sql.getQuery());
		return this;
	}
	
	/**
	 * Sets the SQL statement text.
	 *
	 * @param query The SQL statement.
	 * @return this fluent instance.
	 */
	public @Nonnull FluentSQL query(@Nonnull String query) {
		sql.setQuery(query);
		return this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The SQL metadata instance.
	 */
	@Override
	public @Nonnull SQLMetaData get() {
		return sql;
	}
}
