package org.skyve.domain.types.formatters;

import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.skyve.metadata.FormatterName;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	public static @Nullable <T> Formatter<T> get(@Nonnull String name) {
		return (Formatter<T>) FORMATTERS.get(name);
	}
	
	/**
	 * Get the Set of formatter names.
	 * @return	An unmodified set of all formatter names registered
	 */
	public static Set<String> getNames() {
		return Collections.unmodifiableSet(FORMATTERS.keySet());
	}
}
