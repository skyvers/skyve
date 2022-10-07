package org.skyve.metadata.repository;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.view.ViewMetaData;

public interface OnDemandRepository {
	void populateKeys();
	
	@Nonnull Router loadRouter();
	@Nonnull CustomerMetaData loadCustomer(@Nonnull String customerName);
	@Nonnull ModuleMetaData loadModule(@Nullable String customerName, @Nonnull String moduleName);
	@Nonnull DocumentMetaData loadDocument(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName);
	@Nonnull ViewMetaData loadView(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName, @Nonnull String uxui, @Nonnull String viewName); 

	long routerLastModifiedMillis();
	long customerLastModifiedMillis(@Nonnull String customerName);
	long moduleLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName);
	long documentLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName);
	long viewLastModifiedMillis(@Nullable String customerName, @Nonnull String moduleName, @Nonnull String documentName, @Nonnull String uxui, @Nonnull String viewName);
}
