package org.skyve;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Locale;
import java.util.SortedMap;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.skyve.domain.number.NumberGenerator;
import org.skyve.domain.types.formatters.Formatter;
import org.skyve.domain.types.formatters.Formatters;
import org.skyve.impl.domain.number.NumberGeneratorStaticSingleton;
import org.skyve.impl.metadata.controller.CustomisationsStaticSingleton;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.controller.Customisations;
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
	public static @Nonnull Persistence getPersistence() {
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
	public static @Nonnull Ordering newOrdering(@Nonnull String by, @Nonnull SortDirection sort) {
		return new CollectionImpl.OrderingImpl(by, sort);
	}
	
	/**
	 * Get the Skyve number generator.
	 * @return	A NumberGenerator.
	 */
	public static @Nonnull NumberGenerator getNumberGenerator() {
		return NumberGeneratorStaticSingleton.get();
	}
	
	/**
	 * Get the current authenticated user for this thread of execution.
	 * 
	 * @return The current user.
	 */
	public static @Nonnull User getUser() {
		return AbstractPersistence.get().getUser();
	}

	/**
	 * Get the current customer for this thread of execution.
	 * 
	 * @return The current customer.
	 */
	public static @Nonnull Customer getCustomer() {
		return AbstractPersistence.get().getUser().getCustomer();
	}

	/**
	 * A place (thread-local), where state can be stashed for the duration of the conversation.
	 * Bear in mind that this map is serialised and cached in the conversation so manage its size aggressively.
	 */
	public static @Nonnull SortedMap<String, Object> getStash() {
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
	public static @Nonnull Repository getRepository() {
		return ProvidedRepositoryFactory.get();
	}
	
	/**
	 * Get the Skyve Customisations.
	 * @return	A Customisations.
	 */
	public static @Nonnull Customisations getCustomisations() {
		return CustomisationsStaticSingleton.get();
	}

	/**
	 * Format a value a using a standard Skyve formatter
	 * @param name The Formatter Name
	 * @param valueToFormat The value to format
	 * @return	The formatted value.
	 */
	@SuppressWarnings("unchecked")
	public static <T> @Nonnull String format(@Nonnull FormatterName name, @Nullable T valueToFormat) {
		if (valueToFormat == null) {
			return "";
		}
		return ((Formatter<T>) name.getFormatter()).toDisplayValue(valueToFormat);
	}

	/**
	 * Format a value a using a custom formatter (added by Customisations in JSON)
	 * @param name The Formatter name
	 * @param valueToFormat The value to format
	 * @return	The formatted value.
	 */
	public static <T> @Nonnull String format(@Nonnull String name, @Nullable T valueToFormat) {
		if (valueToFormat == null) {
			return "";
		}
		Formatter<T> formatter = Formatters.get(name);
		if (formatter == null) {
			return valueToFormat.toString();
		}
		return formatter.toDisplayValue(valueToFormat);
	}

	/**
	 * Get a date format for the current thread of execution.
	 * Since format objects are not thread-safe, this convenience method exists to return a new one each time.
	 * 
	 * @return A date format.
	 */
	public static @Nonnull SimpleDateFormat getDateFormat(@Nonnull String formatString) {
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
	public static @Nonnull DecimalFormat getDecimalFormat(@Nonnull String formatString) {
		return new DecimalFormat(formatString);
	}
	
	public static @Nonnull SimpleDateFormat getSerializableDateFormat() {
		return getDateFormat("yyyy-MM-dd");
	}

	public static @Nonnull SimpleDateFormat getSerializableTimeFormat() {
		return getDateFormat("HH:mm:ss");
	}
}
