package org.skyve.metadata.controller;

import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpSession;

/**
 * Enables observing Skyve application lifecycle, backup, restore, and session
 * events.
 *
 * <p>
 * Application startup occurs in two phases. {@link #startup(Customer)}
 * runs synchronously after Skyve's services have started and before
 * metadata validation, allowing observers to populate or customise Skyve or
 * system metadata. {@link #dataTierReady(Customer)} runs asynchronously after
 * content startup and bootstrap, with a super user persistence context
 * available to the calling thread.
 *
 * @author mike
 */
public interface Observer {
	/**
	 * Called during server startup after Skyve services have started and before
	 * metadata validation.
	 *
	 * <p>
	 * No persistence context is available to the calling thread. This callback can
	 * populate metadata and customisations before they are validated.
	 *
	 * @param customer The customer observing.
	 */
	void startup(@Nonnull Customer customer);

	/**
	 * Called during asynchronous data tier startup after persistence and content manager have started
	 * and Skyve bootstrap has completed.
	 *
	 * <p>
	 * A persistence context with a Super User is available in the thread.
	 *
	 * @param customer The customer observing.
	 */
	void dataTierReady(@Nonnull Customer customer);

	/**
	 * Called when shutting down a skyve application. All Skyve services are still
	 * available in this call but there will be no persistence for the thread.
	 * 
	 * @param customer The customer observing.
	 */
	void shutdown(@Nonnull Customer customer);

	/**
	 * Called within a backup job run for a customer before the backup work begins.
	 * The backup job persistence is available.
	 * 
	 * @param customer The customer observing.
	 */
	void beforeBackup(@Nonnull Customer customer);

	/**
	 * Called within a backup job run for a customer after the backup work completes
	 * (in a finally block). The backup job persistence is available.
	 * 
	 * @param customer The customer observing.
	 */
	void afterBackup(@Nonnull Customer customer);

	/**
	 * Called within a restore job run for a customer before the restore work
	 * begins. The restore job persistence is available.
	 * 
	 * @param customer The customer observing.
	 */
	void beforeRestore(@Nonnull Customer customer);

	/**
	 * Called within a restore job run for a customer after the restore work
	 * completes (in a finally block). The restore job persistence is available.
	 * 
	 * @param customer The customer observing.
	 */
	void afterRestore(@Nonnull Customer customer);

	/**
	 * Called after login has occurred and the session has been established.
	 * 
	 * @param user    The user who just logged in.
	 * @param session The established session.
	 */
	void login(@Nonnull User user, @Nonnull HttpSession session);

	/**
	 * Called after logout or session expiration has occurred and the session is
	 * about to be destroyed.
	 * 
	 * @param user    The user who just logged out or who's session just expired.
	 * @param session The session to be destroyed.
	 */
	void logout(@Nonnull User user, @Nonnull HttpSession session);
}
