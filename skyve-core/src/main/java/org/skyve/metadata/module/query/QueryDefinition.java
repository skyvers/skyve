package org.skyve.metadata.module.query;

import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Base contract for all query definitions used in Skyve list views and programmatic queries.
 *
 * <p>A {@code QueryDefinition} declares metadata shared by every query type: an owning
 * module, a display description, optional documentation prose, and a query-execution
 * timeout. Concrete subtypes specialise the query payload:
 * <ul>
 *   <li>{@link MetaDataQueryDefinition} — document-driven structured query with column metadata</li>
 *   <li>{@link BizQLDefinition} — Skyve BizQL string query</li>
 *   <li>{@link SQLDefinition} — native SQL string query</li>
 * </ul>
 *
 * <p>Query definitions are declared in module XML metadata and loaded by the metadata
 * repository. They are resolved by name via
 * {@link org.skyve.metadata.module.Module#getMetaDataQuery} and related methods.
 *
 * @see MetaDataQueryDefinition
 * @see BizQLDefinition
 * @see SQLDefinition
 */
public interface QueryDefinition extends NamedMetaData, DecoratedMetaData  {
	/**
	 * Returns the module that owns this query definition.
	 *
	 * @return the owning module; never {@code null}
	 */
	@Nonnull Module getOwningModule();

	/**
	 * Returns the human-readable description of this query.
	 *
	 * <p>The description is typically displayed as a tooltip or label in the UI.
	 * For a localised version use {@link #getLocalisedDescription()}.
	 *
	 * @return the query description; never {@code null}
	 */
	@Nonnull String getDescription();
	
	/**
	 * Returns the localised description of this query for the current user locale.
	 *
	 * <p>Delegates to {@link org.skyve.util.Util#nullSafeI18n(String)} using
	 * {@link #getDescription()} as the resource key.
	 *
	 * @return a non-{@code null} localised description; falls back to the raw key
	 */
	default @Nonnull String getLocalisedDescription() {
		return Util.nullSafeI18n(getDescription());
	}
	
	/**
	 * Returns optional long-form documentation for this query.
	 *
	 * <p>Documentation is free-form prose intended for developer or administrator
	 * reference, not end-user display.
	 *
	 * @return the documentation string, or {@code null} if none is specified
	 */
	@Nullable String getDocumentation();
	
	/**
	 * Returns the query execution timeout in seconds.
	 *
	 * <p>A value of {@code 0} means no explicit timeout is set and the framework
	 * defers to the global Skyve session or persistence timeout configuration.
	 *
	 * @return the timeout in seconds; {@code 0} means use the Skyve default
	 */
	int getTimeoutInSeconds();
}
