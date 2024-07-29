package org.skyve.metadata.controller;

import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpSession;

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
	void startup(@Nonnull Customer customer);

	/**
	 * Called when shutting down a skyve application.
	 * All Skyve services are still available in this call but there will be no persistence for the thread.
	 * @param customer	The customer observing.
	 */
	void shutdown(@Nonnull Customer customer);
	
	/**
	 * Called within a restore job run for a customer before the restore work begins.
	 * The restore job persistence is available.
	 * @param customer	The customer observing.
	 */
	void beforeRestore(@Nonnull Customer customer);

	/**
	 * Called within a restore job run for a customer after the restore work completes (in a finally block).
	 * The restore job persistence is available.
	 * @param customer	The customer observing.
	 */
	void afterRestore(@Nonnull Customer customer);
	
	/**
	 * Called after login has occurred and the session has been established.
	 * @param user	The user who just logged in.
	 * @param session	The established session.
	 */
	void login(@Nonnull User user, @Nonnull HttpSession session);
	
	/**
	 * Called after logout or session expiration has occurred and the session is about to be destroyed.
	 * @param user	The user who just logged out or who's session just expired.
	 * @param session	The session to be destroyed.
	 */
	void logout(@Nonnull User user, @Nonnull HttpSession session);
}
