package org.skyve.metadata.repository;

import javax.annotation.Nonnull;

import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.View;

public interface MutableRepository {
	@Nonnull Router setRouter(@Nonnull Router router);
	@Nonnull Customer addCustomer(@Nonnull CustomerMetaData customer);
	@Nonnull Module addModule(@Nonnull Customer customer, @Nonnull ModuleMetaData module);
	@Nonnull Module addModule(@Nonnull ModuleMetaData module);
	@Nonnull Document addDocument(@Nonnull Customer customer, @Nonnull Module module, @Nonnull DocumentMetaData document);
	@Nonnull Document addDocument(@Nonnull Module module, @Nonnull DocumentMetaData document);
	@Nonnull View addView(@Nonnull String uxui, @Nonnull Customer customer, @Nonnull Document document, @Nonnull ViewMetaData view); 
	@Nonnull View addView(@Nonnull String uxui, @Nonnull Document document, @Nonnull ViewMetaData view); 
}
