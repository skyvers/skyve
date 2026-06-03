package org.skyve.metadata.repository;

import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.view.ViewMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * SPI for lazy, on-demand loading of raw metadata artefacts from the backing store
 * (file system, database, classpath, etc.).
 *
 * <p>An {@code OnDemandRepository} is the read side of the metadata store abstraction.
 * Implementations load each artefact type on demand and report its last-modified
 * timestamp so the caching layer ({@link CachedRepository}) can decide whether a
 * reload is necessary.
 *
 * <p>All {@code load*} methods return fully-parsed (but not yet resolved) raw metadata
 * objects that the {@link MutableRepository} then converts to the runtime representation.
 *
 * @see MutableRepository
 * @see CachedRepository
 */
public interface OnDemandRepository {
	/**
	 * Populates the repository's internal key set with all known artefact identifiers
	 * (customer names, module names, document names, view names, etc.) so that
	 * {@link CachedRepository} can enumerate available artefacts without loading each one.
	 *
	 * <p>Implementations typically scan a directory structure, database table, or
	 * manifest file.
	 */
	void populateKeys();

	/**
	 * Loads and returns the router metadata.
	 *
	 * @return the parsed router; never {@code null}
	 */
	@Nonnull Router loadRouter();

	/**
	 * Loads and returns the customer metadata for {@code customerName}.
	 *
	 * @param customerName the customer name; must not be {@code null}
	 * @return the parsed customer metadata; never {@code null}
	 */
	@Nonnull CustomerMetaData loadCustomer(@Nonnull String customerName);

	/**
	 * Loads and returns the module metadata for {@code moduleName}, optionally scoped
	 * to a specific customer for customer-overridden modules.
	 *
	 * @param customerName the customer name, or {@code null} for the default module
	 * @param moduleName   the module name; must not be {@code null}
	 * @return the parsed module metadata; never {@code null}
	 */
	@Nonnull ModuleMetaData loadModule(@Nullable String customerName, @Nonnull String moduleName);

	/**
	 * Loads and returns the document metadata for {@code documentName} within the
	 * given module.
	 *
	 * @param customerName the customer name, or {@code null} for the default document
	 * @param moduleName   the module name; must not be {@code null}
	 * @param documentName the document name; must not be {@code null}
	 * @return the parsed document metadata; never {@code null}
	 */
	@Nonnull DocumentMetaData loadDocument(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName);

	/**
	 * Loads and returns the view metadata for {@code viewName} on the given document,
	 * optionally scoped to a specific UX/UI variant.
	 *
	 * @param customerName the customer name, or {@code null} for the default view
	 * @param moduleName   the module name; must not be {@code null}
	 * @param documentName the document name; must not be {@code null}
	 * @param uxui         the UX/UI variant such as {@code "desktop"} or {@code "responsive"},
	 *                     or {@code null} for the default variant
	 * @param viewName     the view name; must not be {@code null}
	 * @return the parsed view metadata; never {@code null}
	 */
	@Nonnull ViewMetaData loadView(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName, @Nullable String uxui, @Nonnull String viewName);

	/**
	 * Loads and returns the action metadata for {@code actionName} on the given document.
	 *
	 * @param customerName the customer name, or {@code null} for the default action
	 * @param moduleName   the module name; must not be {@code null}
	 * @param documentName the document name; must not be {@code null}
	 * @param actionName   the action name; must not be {@code null}
	 * @return the parsed action metadata; never {@code null}
	 */
	@Nonnull ActionMetaData loadMetaDataAction(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName, @Nonnull String actionName);

	/**
	 * Loads and returns the bizlet metadata for the given document.
	 *
	 * @param customerName the customer name, or {@code null} for the default bizlet
	 * @param moduleName   the module name; must not be {@code null}
	 * @param documentName the document name; must not be {@code null}
	 * @return the parsed bizlet metadata; never {@code null}
	 */
	@Nonnull BizletMetaData loadMetaDataBizlet(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName);

	/**
	 * Returns the last-modified timestamp of the router source, in milliseconds since
	 * the epoch.
	 *
	 * @return last-modified time, or {@code 0} if not available
	 */
	long routerLastModifiedMillis();

	/**
	 * Returns the last-modified timestamp of the customer source for
	 * {@code customerName}, in milliseconds since the epoch.
	 *
	 * @param customerName the customer name; must not be {@code null}
	 * @return last-modified time, or {@code 0} if not available
	 */
	long customerLastModifiedMillis(@Nonnull String customerName);

	/**
	 * Returns the last-modified timestamp of the module source for
	 * {@code moduleName}, in milliseconds since the epoch.
	 *
	 * @param customerName the customer name, or {@code null} for the default module
	 * @param moduleName   the module name; must not be {@code null}
	 * @return last-modified time, or {@code 0} if not available
	 */
	long moduleLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName);

	/**
	 * Returns the last-modified timestamp of the document source for
	 * {@code documentName} within the given module, in milliseconds since the epoch.
	 *
	 * @param customerName the customer name, or {@code null} for the default document
	 * @param moduleName   the module name; must not be {@code null}
	 * @param documentName the document name; must not be {@code null}
	 * @return last-modified time, or {@code 0} if not available
	 */
	long documentLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName);

	/**
	 * Returns the last-modified timestamp of the view source for {@code viewName}
	 * on the given document, in milliseconds since the epoch.
	 *
	 * @param customerName the customer name, or {@code null} for the default view
	 * @param moduleName   the module name; must not be {@code null}
	 * @param documentName the document name; must not be {@code null}
	 * @param uxui         the UX/UI variant, or {@code null} for the default variant
	 * @param viewName     the view name; must not be {@code null}
	 * @return last-modified time, or {@code 0} if not available
	 */
	long viewLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName, @Nullable String uxui, @Nonnull String viewName);

	/**
	 * Returns the last-modified timestamp of the action source for
	 * {@code actionName} on the given document, in milliseconds since the epoch.
	 *
	 * @param customerName the customer name, or {@code null} for the default action
	 * @param moduleName   the module name; must not be {@code null}
	 * @param documentName the document name; must not be {@code null}
	 * @param actionName   the action name; must not be {@code null}
	 * @return last-modified time, or {@code 0} if not available
	 */
	long metaDataActionLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName, @Nonnull String actionName);

	/**
	 * Returns the last-modified timestamp of the bizlet source for the given
	 * document, in milliseconds since the epoch.
	 *
	 * @param customerName the customer name, or {@code null} for the default bizlet
	 * @param moduleName   the module name; must not be {@code null}
	 * @param documentName the document name; must not be {@code null}
	 * @return last-modified time, or {@code 0} if not available
	 */
	long metaDataBizletLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName);
}
