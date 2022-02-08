package org.skyve;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Locale;
import java.util.SortedMap;

import org.skyve.domain.number.NumberGenerator;
import org.skyve.impl.domain.number.NumberGeneratorStaticSingleton;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Collection.Ordering;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

/**
 * The central factory for creating all objects required in the skyve core API.
 * See {@link org.skyve.EXT} for creating objects implemented in skyve ext.
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
		return new CollectionImpl.OrderingImpl(by, sort);
	}
	
	/**
	 * Get the Skyve number generator.
	 * @return	A NumberGenerator.
	 */
	public static NumberGenerator getNumberGenerator() {
		return NumberGeneratorStaticSingleton.get();
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
	 * Get the current customer for this thread of execution.
	 * 
	 * @return The current customer.
	 */
	public static Customer getCustomer() {
		return AbstractPersistence.get().getUser().getCustomer();
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
	 * This is used to access all skyve's metadata programmatically.
	 * Most metadata is available from {@link org.skyve.metadata.customer.Customer}
	 * through <code>CORE.getUser().getCustomer()<code> or <code>persistence.getUser().getCustomer()</code>.
	 * 
	 * @return The repository.
	 */
	public static Repository getRepository() {
		return ProvidedRepositoryFactory.get();
	}
	
	/**
	 * Get a date format for the current thread of execution.
	 * Since format objects are not thread-safe, this convenience method exists to return a new one each time.
	 * 
	 * @return A date format.
	 */
	public static SimpleDateFormat getDateFormat(String formatString) {
		SimpleDateFormat result = new SimpleDateFormat(formatString, Locale.ENGLISH);
		result.setLenient(false);
		return result;
	}
	
    /**
     * Get a decimal format for the current thread of execution.
     * Since format objects are not thread-safe, this convenience method exists to return a new one each time.
     * 
     * @return A decimal format.
     */
	public static DecimalFormat getDecimalFormat(String formatString) {
		return new DecimalFormat(formatString);
	}
	
	public static SimpleDateFormat getSerializableDateFormat() {
		return getDateFormat("yyyy-MM-dd");
	}

	public static SimpleDateFormat getSerializableTimeFormat() {
		return getDateFormat("HH:mm:ss");
	}
}
