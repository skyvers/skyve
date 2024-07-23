package org.skyve.metadata.view.model;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.customer.Customer;

/**
 * Extends metaData to represent any coded view models.
 * postConstruct() is called by Skyve with the customer at runtime and generate time.
 */
public interface ViewModel extends MetaData {
	/**
	 * Called by Skyve after instantiation to allow the model to set up 
	 * based on whether it is required for runtime (injected with Skyve services available)
	 * or at generate time (just to load and validate the model).
	 */
	void postConstruct(Customer customer, boolean runtime);
}
