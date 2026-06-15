package org.skyve.metadata.module.query;

import jakarta.annotation.Nonnull;

/**
 * A query definition backed by a native SQL string.
 *
 * <p>The SQL string is retrieved via {@link #getQuery()} and executed directly against
 * the underlying database. SQL queries bypass the ORM layer and are therefore not
 * portable across database vendors without modification.
 *
 * <p>SQL queries are declared in module XML metadata and looked up by name via
 * {@link org.skyve.metadata.module.Module#getMetaDataQuery}.
 *
 * @see QueryDefinition
 * @see BizQLDefinition
 */
public interface SQLDefinition extends QueryDefinition {
	/**
	 * Returns the native SQL query string for this definition.
	 *
	 * @return the SQL query string; never {@code null}
	 */
	@Nonnull String getQuery();
}
