package org.skyve.metadata.controller;

import org.skyve.metadata.customer.Customer;

/**
 * Enables observing skyve application events - currently startup and shutdown.
 * @author mike
 */
public interface Observer {
	/**
	 * Called when starting up a skyve application.
	 * All Skyve services will have started by this call but there will be no persistence for the thread.
	 * @param customer	The customer observing.
	 */
	void startup(Customer customer);

	/**
	 * Called when shutting down a skyve application.
	 * All Skyve services are still available in this call but there will be no persistence for the thread.
	 * @param customer	The customer observing.
	 */
	void shutdown(Customer customer);
	
	/**
	 * Called within a restore job run for a customer before the restore work begins.
	 * The restore job persistence is available.
	 * @param customer	The customer observing.
	 */
	void preRestore(Customer customer);

	/**
	 * Called within a restore job run for a customer after the restore work completes (in a finally block).
	 * The restore job persistence is available.
	 * @param customer	The customer observing.
	 */
	void postRestore(Customer customer);
}
