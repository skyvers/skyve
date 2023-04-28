package org.skyve.metadata.controller;

import org.skyve.metadata.customer.Customer;

/**
 * Enables observing skyve application events - currently startup and shutdown.
 * @author mike
 */
public interface Observer {
	/**
	 * Called when starting up a skyve application.
	 * @param customer	The customer observing.
	 */
	void startup(Customer customer);

	/**
	 * Called when shutting down a skyve application.
	 * @param customer	The customer observing.
	 */
	void shutdown(Customer customer);
}
