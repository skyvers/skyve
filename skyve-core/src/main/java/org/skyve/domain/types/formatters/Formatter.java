package org.skyve.domain.types.formatters;

import jakarta.annotation.Nonnull;

/**
 * Specify a class that can create a formatted String from a value of a certain type.
 * Types of this interface should be thread-safe as only 1 instance of the class is constructed by Skyve and is used concurrently.
 * Note: Use CORE.get...Format() classes to obtain java.text.Formatter instances in implementations as they are also not thread safe.
 */
public interface Formatter<T> {
	/**
	 * The broadest type of value this formatter can format.
	 * @return	The type as a java class.
	 */
	@Nonnull Class<T> getValueType();

	/**
	 * Convert an instance of T to a display value.
	 * Should be thread-safe as only 1 instance of the class is constructed by Skyve and is used concurrently.
	 * This method should not be used directly but should be delegated to through CORE.format() methods.
	 * @param value	An instance of T.
	 * @return	The display value.
	 */
	@Nonnull String toDisplayValue(@Nonnull T value);
}
