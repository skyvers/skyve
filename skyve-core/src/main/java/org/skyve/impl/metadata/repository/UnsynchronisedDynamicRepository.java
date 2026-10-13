package org.skyve.impl.metadata.repository;

import java.util.Collections;
import java.util.List;

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
 * Extend or use this repository for dynamic domains where there is no on-demand loading.
 * This implementation does not have a thread safe cache and mutations should happen before the repository is placed in the chain.
 * Use this implementation when preloading all metadata on startup.
 *
 * Stores dynamically supplied metadata without on-demand file loading.
 *
 * <p>This implementation expects callers to preload metadata programmatically via
 * {@code put*()} methods before the repository is placed in the chain. All
 * {@code load*()} methods intentionally return {@code null} and all
 * {@code *LastModifiedMillis()} methods return {@link Long#MIN_VALUE} so that
 * dev-mode reload checks never trigger.
 *
 * <p>Threading: not thread-safe for mutations. Populate the repository during
 * startup, then publish it for read-only access.
 */
public class UnsynchronisedDynamicRepository extends AbstractDynamicRepository {
	/**
	 * Performs no key discovery because metadata is injected programmatically.
	 */
	@Override
	public void populateKeys() {
		// nothing to do as metadata will be added programmatically
	}

	/**
	 * Returns {@code null} because no router is loaded from an external source.
	 */
	@Override
	public @Nullable Router loadRouter() {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	/**
	 * Returns {@link Long#MIN_VALUE} to disable reload checks for router metadata.
	 */
	@Override
	public long routerLastModifiedMillis() {
		// never reload as metadata will be added programmatically
		return Long.MIN_VALUE;
	}
	
	/**
	 * Returns {@code null} because customer metadata is not loaded from external storage.
	 *
	 * @param customerName the customer identifier
	 * @return always {@code null}
	 */
	@Override
	public @Nullable CustomerMetaData loadCustomer(@Nonnull String customerName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	/**
	 * Returns {@link Long#MIN_VALUE} so customer metadata is never considered stale.
	 *
	 * @param customerName the customer identifier
	 * @return always {@link Long#MIN_VALUE}
	 */
	@Override
	public long customerLastModifiedMillis(@Nonnull String customerName) {
		// never reload as metadata will be added programmatically
		return Long.MIN_VALUE;
	}
	
	/**
	 * Returns {@code null} because module metadata is not loaded from external storage.
	 *
	 * @param customerName the customer identifier
	 * @param moduleName the module identifier
	 * @return always {@code null}
	 */
	@Override
	public @Nullable ModuleMetaData loadModule(@Nullable String customerName, @Nonnull String moduleName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	/**
	 * Returns {@link Long#MIN_VALUE} so module metadata is never considered stale.
	 *
	 * @param customerName the customer identifier
	 * @param moduleName the module identifier
	 * @return always {@link Long#MIN_VALUE}
	 */
	@Override
	public long moduleLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName) {
		// never reload as metadata will be added programmatically
		return Long.MIN_VALUE;
	}
	
	/**
	 * Returns {@code null} because document metadata is not loaded from external storage.
	 *
	 * @param customerName the customer identifier
	 * @param moduleName the module identifier
	 * @param documentName the document identifier
	 * @return always {@code null}
	 */
	@Override
	public @Nullable DocumentMetaData loadDocument(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	/**
	 * Returns {@link Long#MIN_VALUE} so document metadata is never considered stale.
	 *
	 * @param customerName the customer identifier
	 * @param moduleName the module identifier
	 * @param documentName the document identifier
	 * @return always {@link Long#MIN_VALUE}
	 */
	@Override
	public long documentLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName) {
		// never reload as metadata will be added programmatically
		return Long.MIN_VALUE;
	}
	
	/**
	 * Returns {@code null} because view metadata is not loaded from external storage.
	 *
	 * @param customerName the customer identifier
	 * @param moduleName the module identifier
	 * @param documentName the document identifier
	 * @param uxui the UX/UI variant key
	 * @param viewName the view identifier
	 * @return always {@code null}
	 */
	@Override
	public @Nullable ViewMetaData loadView(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName, @Nullable String uxui, @Nonnull String viewName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	/**
	 * Returns {@link Long#MIN_VALUE} so view metadata is never considered stale.
	 *
	 * @param customerName the customer identifier
	 * @param moduleName the module identifier
	 * @param documentName the document identifier
	 * @param uxui the UX/UI variant key
	 * @param viewName the view identifier
	 * @return always {@link Long#MIN_VALUE}
	 */
	@Override
	public long viewLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName, @Nullable String uxui, @Nonnull String viewName) {
		// never reload as metadata will be added programmatically
		return Long.MIN_VALUE;
	}
	
	/**
	 * Returns {@code null} because action metadata is not loaded from external storage.
	 *
	 * @param customerName the customer identifier
	 * @param moduleName the module identifier
	 * @param documentName the document identifier
	 * @param actionName the action identifier
	 * @return always {@code null}
	 */
	@Override
	public @Nullable ActionMetaData loadMetaDataAction(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName, @Nonnull String actionName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	/**
	 * Returns {@link Long#MIN_VALUE} so action metadata is never considered stale.
	 *
	 * @param customerName the customer identifier
	 * @param moduleName the module identifier
	 * @param documentName the document identifier
	 * @param actionName the action identifier
	 * @return always {@link Long#MIN_VALUE}
	 */
	@Override
	public long metaDataActionLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName, @Nonnull String actionName) {
		// never reload as metadata will be added programmatically
		return Long.MIN_VALUE;
	}

	/**
	 * Returns {@code null} because Bizlet metadata is not loaded from external storage.
	 *
	 * @param customerName the customer identifier
	 * @param moduleName the module identifier
	 * @param documentName the document identifier
	 * @return always {@code null}
	 */
	@Override
	public @Nullable BizletMetaData loadMetaDataBizlet(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName) {
		// nothing to do as metadata will be added programmatically
		return null;
	}

	/**
	 * Returns {@link Long#MIN_VALUE} so Bizlet metadata is never considered stale.
	 *
	 * @param customerName the customer identifier
	 * @param moduleName the module identifier
	 * @param documentName the document identifier
	 * @return always {@link Long#MIN_VALUE}
	 */
	@Override
	public long metaDataBizletLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName) {
		// never reload as metadata will be added programmatically
		return Long.MIN_VALUE;
	}

	/**
	 * Returns an empty list because this repository does not discover customer folders.
	 */
	@Override
	public @Nonnull List<String> getAllCustomerNames() {
		// TODO should I find the customer cache keys?
		return Collections.emptyList();
	}

	/**
	 * Returns an empty list because this repository does not discover module folders.
	 */
	@Override
	public @Nonnull List<String> getAllVanillaModuleNames() {
		// TODO should I find the vanilla module cache keys?
		return Collections.emptyList();
	}

	/**
	 * Indicates that scaffolded fallback views are disabled for dynamic repositories.
	 */
	@Override
	public boolean getUseScaffoldedViews() {
		return false;
	}
}
