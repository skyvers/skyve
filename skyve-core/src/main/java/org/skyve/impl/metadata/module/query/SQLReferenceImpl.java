package org.skyve.impl.metadata.module.query;

import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.module.query.SQLDefinition;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A query reference that points to an existing {@link SQLDefinition} by name.
 *
 * <p>Used in widget definitions to reuse a named SQL query from the same
 * module without duplicating its statement.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see QueryReferenceImpl
 * @see SQLDefinition
 */
public class SQLReferenceImpl extends QueryReferenceImpl implements SQLDefinition {
	private static final long serialVersionUID = -5989614154659932867L;

	public SQLReferenceImpl(@Nullable String name, @Nonnull String moduleRef, @Nonnull String ref) {
		super(name, moduleRef, ref);
	}

	@Override
	public @Nonnull String getQuery() {
		return getTarget().getQuery();
	}

	@Override
	@SuppressWarnings("unchecked")
	protected @Nonnull SQLDefinition getTarget() {
		// TODO this should use the current customer (can't get customer during generate domain)
		return ProvidedRepositoryFactory.getNullSafe().getModule(null, moduleRef).getSQL(ref);
	}
}
