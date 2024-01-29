package org.skyve.metadata.repository;

import java.io.File;

import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * The application interface for the meta data repository.
 */
public interface Repository {
	/**
	 * Check in customer module folder, check in module folder, check in customer images folder, check in images folder.
	 * 
	 * @param imagePath The relative path to the image
	 * @param customerName The name of the customer.
	 * @param moduleName The name of the module.
	 * @return The resource file.
	 */
	@Nonnull File findResourceFile(@Nonnull String resourcePath, @Nullable String customerName, @Nullable String moduleName);
	
	/**
	 * @return A merged Router consisting of the main router and all of the module routers.
	 */
	@Nonnull Router getRouter();

	/**
	 * 
	 * @param customerName
	 * @return
	 */
	@Nullable Customer getCustomer(@Nonnull String customerName);

	default @Nullable Object getDataFactory(@Nonnull Customer customer, @Nonnull String moduleName, @Nonnull String documentName) {
		Module m = customer.getModule(moduleName);
		Document d = m.getDocument(customer, documentName);
		return getDataFactory(customer, d);
	}
	
	@Nullable Object getDataFactory(@Nullable Customer customer, @Nonnull Document document);
	
	/**
	 * 
	 * @param userName
	 * @return
	 */
	@Nullable User retrieveUser(@Nonnull String userName);
}
