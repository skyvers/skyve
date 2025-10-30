package org.skyve.util;

/**
 * Utility class providing null-safe coalescing operations.
 * This class cannot be instantiated and provides static methods for handling null values.
 */
public class Coalesce {

	private Coalesce() {
		// Make the class non-instantiable
	}

	/**
	 * Returns the first non-null value between the two provided parameters.
	 * 
	 * @param <T> the type of the values
	 * @param val the primary value to check
	 * @param ifNullValue the fallback value to return if val is null
	 * @return val if it is not null, otherwise ifNullValue
	 */
	public static <T> T coalesceNull(T val, T ifNullValue) {
		return (val == null ? ifNullValue : val);
	}
}
