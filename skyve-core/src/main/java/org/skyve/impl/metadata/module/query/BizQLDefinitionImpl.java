package org.skyve.impl.metadata.module.query;

import org.skyve.metadata.module.query.BizQLDefinition;

import jakarta.annotation.Nonnull;

/**
 * Runtime implementation of {@link BizQLDefinition} — a BizQL query declared
 * in a module descriptor.
 *
 * <p>BizQL is Skyve's object-oriented query language, similar to JPQL but
 * with document-oriented binding paths.  Holds the raw BizQL statement string
 * that is compiled and executed by the persistence layer.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see QueryDefinitionImpl
 * @see BizQLDefinition
 */
public class BizQLDefinitionImpl extends QueryDefinitionImpl implements BizQLDefinition {
	private static final long serialVersionUID = -6010414111423395137L;

	private String query;

	@Override
	public String getQuery() {
		return query;
	}

	public void setQuery(@Nonnull String query) {
		this.query = query;
	}
}
