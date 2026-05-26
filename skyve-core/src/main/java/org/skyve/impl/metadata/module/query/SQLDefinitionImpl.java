package org.skyve.impl.metadata.module.query;

import org.skyve.metadata.module.query.SQLDefinition;

import jakarta.annotation.Nonnull;

/**
 * Runtime implementation of {@link SQLDefinition} — a native SQL query declared
 * in a module descriptor.
 *
 * <p>Holds the raw SQL statement string that is executed directly against the
 * database.  The framework does not parse or validate the SQL at load time;
 * validation occurs at execution.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see QueryDefinitionImpl
 * @see SQLDefinition
 */
public class SQLDefinitionImpl extends QueryDefinitionImpl implements SQLDefinition {
	private static final long serialVersionUID = 4044590120129931022L;

	private String query;

	@Override
	public String getQuery() {
		return query;
	}

	public void setQuery(@Nonnull String query) {
		this.query = query;
	}
}
