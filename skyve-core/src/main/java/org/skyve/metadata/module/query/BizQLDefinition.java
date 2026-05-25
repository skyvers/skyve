package org.skyve.metadata.module.query;

import jakarta.annotation.Nonnull;

/**
 * A query definition backed by a Skyve BizQL string.
 *
 * <p>BizQL is Skyve's HQL-like query language that operates over the persistent domain
 * model. The query string is retrieved via {@link #getQuery()} and executed against the
 * current persistence context. Variable substitution and parameter binding are performed
 * by the BizQL execution engine.
 *
 * <p>BizQL queries are declared in module XML metadata and looked up by name via
 * {@link org.skyve.metadata.module.Module#getMetaDataQuery}.
 *
 * @see QueryDefinition
 * @see SQLDefinition
 */
public interface BizQLDefinition extends QueryDefinition {
	/**
	 * Returns the BizQL query string for this definition.
	 *
	 * <p>The string is a Skyve BizQL expression (HQL dialect) and may reference
	 * named parameters using the {@code :{name}} syntax.
	 *
	 * @return the BizQL query string; never {@code null}
	 */
	@Nonnull String getQuery();
}
