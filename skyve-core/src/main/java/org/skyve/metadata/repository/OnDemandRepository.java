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
	void populateKeys();
	
	@Nonnull Router loadRouter();
	@Nonnull CustomerMetaData loadCustomer(@Nonnull String customerName);
	@Nonnull ModuleMetaData loadModule(@Nullable String customerName, @Nonnull String moduleName);
	@Nonnull DocumentMetaData loadDocument(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName);
	@Nonnull ViewMetaData loadView(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName, @Nonnull String uxui, @Nonnull String viewName); 
	@Nonnull ActionMetaData loadMetaDataAction(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName, @Nonnull String actionName); 
	@Nonnull BizletMetaData loadMetaDataBizlet(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName);

	long routerLastModifiedMillis();
	long customerLastModifiedMillis(@Nonnull String customerName);
	long moduleLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName);
	long documentLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName);
	long viewLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName, @Nonnull String uxui, @Nonnull String viewName);
	long metaDataActionLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName, @Nonnull String actionName);
	long metaDataBizletLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName);
}
