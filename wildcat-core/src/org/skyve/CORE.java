package org.skyve;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.SortedMap;

import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Collection.Ordering;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.ThreadSafeFactory;

/**
 * The central factory for creating all objects required in the wildcat core API.
 * See {@link org.skyve.EXT} for creating objects implemented in wildcat ext.
 */
public class CORE {
	/**
	 * Disallow instantiation
	 */
	private CORE() {
		// no-op
	}
	
	/**
	 * Get a persistence object associated with the current thread of execution.
     * Note that all persistence related objects can get created through a 
     * {@link org.skyve.persistence.Persistence}.
	 * 
	 * @return The persistence object.
	 */
	public static Persistence getPersistence() {
		return AbstractPersistence.get();
	}
	
	/**
	 * Create a new Ordering object used in {@link org.skyve.util.Binder}
	 * and in {@link org.skyve.persistence.Query}s.
	 * Note that all persistence related objects can get created through a 
	 * {@link org.skyve.persistence.Persistence}.
	 * 
	 * @return The new ordering specification.
	 */
	public static Ordering newOrdering(String by, SortDirection sort) {
		return new org.skyve.wildcat.metadata.model.document.Collection.Ordering(by, sort);
	}
	
	/**
	 * Get the current authenticated user for this thread of execution.
	 * 
	 * @return The current user.
	 */
	public static User getUser() {
		return AbstractPersistence.get().getUser();
	}

	/**
	 * A place (thread-local), where state can be stashed for the duration of the conversation.
	 * Bear in mind that this map is serialised and cached in the conversation so manage its size aggressively.
	 */
	public static SortedMap<String, Object> getStash() {
		return AbstractPersistence.get().getStash();
	}
	
	/**
	 * Get the repository for this thread of execution.
	 * This is used to access all wildcat's metadata programmatically.
	 * Most metadata is available from {@link org.skyve.metadata.customer.Customer}
	 * through <code>CORE.getUser().getCustomer()<code> or <code>persistence.getUser().getCustomer()</code>.
	 * 
	 * @return The repository.
	 */
	public static Repository getRepository() {
		return org.skyve.wildcat.metadata.repository.AbstractRepository.get();
	}
	
	/**
	 * Get a date format for the current thread of execution.
	 * Since format objects are not thread-safe, this convenience method exists to return one for each thread.
	 * 
	 * @return A date format.
	 */
	public static SimpleDateFormat getDateFormat(String formatString) {
		return ThreadSafeFactory.getDateFormat(formatString);
	}
	
    /**
     * Get a decimal format for the current thread of execution.
     * Since format objects are not thread-safe, this convenience method exists to return one for each thread.
     * 
     * @return A decimal format.
     */
	public static DecimalFormat getDecimalFormat(String formatString) {
		return ThreadSafeFactory.getDecimalFormat(formatString);
	}
}
