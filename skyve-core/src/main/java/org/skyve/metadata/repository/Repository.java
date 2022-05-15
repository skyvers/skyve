package org.skyve.metadata.repository;

import java.io.File;

import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

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
	File findResourceFile(String resourcePath, String customerName, String moduleName);
	
	/**
	 * @return A merged Router consisting of the main router and all of the module routers.
	 */
	Router getRouter();

	/**
	 * 
	 * @param customerName
	 * @return
	 */
	Customer getCustomer(String customerName);

	default Object getDataFactory(Customer customer, String moduleName, String documentName) {
		Module m = customer.getModule(moduleName);
		Document d = m.getDocument(customer, documentName);
		return getDataFactory(customer, d);
	}
	
	Object getDataFactory(Customer customer, Document document);
	
	/**
	 * 
	 * @param userName
	 * @return
	 */
	User retrieveUser(String userName);
}
