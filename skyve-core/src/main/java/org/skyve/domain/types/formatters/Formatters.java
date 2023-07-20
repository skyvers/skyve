package org.skyve.domain.types.formatters;

import java.util.Map;
import java.util.TreeMap;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.skyve.metadata.FormatterName;

/**
 * Static Map of defined formatters.
 */
public class Formatters {
	private static final Map<String, Formatter<?>> FORMATTERS = new TreeMap<>();
	
	static {
		for (FormatterName name : FormatterName.values()) {
			FORMATTERS.put(name.name(), name.getFormatter());
		}
	}
	
	private Formatters( ) {
		// prevent instantiation
	}

	/**
	 * Register a formatter by name.
	 * The formatter name can not already exist.
	 * NB This should be called in a thread-safe way - such as an implementation of Customisations.registerCustomFormatters().
	 */
	public static void register(@Nonnull String name, @Nonnull Formatter<?> formatter) {
		if (FORMATTERS.put(name, formatter) != null) {
			throw new IllegalStateException("Formatter " + name + " is already defined and cannot be registered.");
		}
	}

	/**
	 * Get a formatter by its name.
	 * CORE.format() methods should be preferred over using the formatter directly.
	 */
	@SuppressWarnings("unchecked")
	public static <T> @Nullable Formatter<T> get(@Nonnull String name) {
		return (Formatter<T>) FORMATTERS.get(name);
	}
}
