package org.skyve.domain.number;

/**
 * Generates unique, monotonically increasing document sequence numbers.
 *
 * <p>Skyve applications use document numbers to assign human-readable identifiers to
 * records (e.g. invoice numbers, order codes). A {@code NumberGenerator} provides
 * thread-safe, per-field counters keyed by {@code (moduleName, documentName, fieldName)}.
 *
 * <p>The primary contract is the five-argument {@link #next(String, String, String, String, int)}
 * method; all other overloads are convenience defaults that delegate to it. Implementations
 * must guarantee that concurrent calls return distinct values.
 *
 * <p>If no counter exists for a given key, the implementation typically bootstraps from
 * the maximum value currently stored in the field's column, preventing duplicates with
 * pre-existing data.
 *
 * <p>The generator is accessed via {@link org.skyve.EXT#getNumberGenerator()}.
 *
 * @see org.skyve.EXT#getNumberGenerator()
 */
public interface NumberGenerator {
	/**
	 * Returns the next sequence value as an {@link Integer}, with no prefix or
	 * minimum length.
	 *
	 * @param moduleName   the module name
	 * @param documentName the document name
	 * @param fieldName    the field name that holds the sequence
	 * @return the next integer sequence value
	 */
	default Integer nextInt(String moduleName, String documentName, String fieldName) {
		return Integer.valueOf(Integer.parseInt(next(null, moduleName, documentName, fieldName, 0)));
	}

	/**
	 * Returns the next sequence value as a {@link Long}, with no prefix or
	 * minimum length.
	 *
	 * @param moduleName   the module name
	 * @param documentName the document name
	 * @param fieldName    the field name that holds the sequence
	 * @return the next long sequence value
	 */
	default Long nextLong(String moduleName, String documentName, String fieldName) {
		return Long.valueOf(Long.parseLong(next(null, moduleName, documentName, fieldName, 0)));
	}

	/**
	 * Returns the next sequence value as a plain string with no prefix and no
	 * minimum length padding.
	 *
	 * @param moduleName   the module name
	 * @param documentName the document name
	 * @param fieldName    the field name that holds the sequence
	 * @return the next sequence value string
	 */
	default String next(String moduleName, String documentName, String fieldName) {
		return next(null, moduleName, documentName, fieldName, 0);
	}

	/**
	 * Returns the next sequence value as a string with no prefix, left-padded with
	 * zeros to at least {@code minimumLength} characters.
	 *
	 * @param moduleName   the module name
	 * @param documentName the document name
	 * @param fieldName    the field name that holds the sequence
	 * @param minimumLength the minimum length of the returned string; shorter results are
	 *                      zero-padded on the left
	 * @return the next sequence value string
	 */
	default String next(String moduleName, String documentName, String fieldName, int minimumLength) {
		return next(null, moduleName, documentName, fieldName, minimumLength);
	}
	
	/**
	 * Returns the next sequence value as a string with the given prefix but no
	 * minimum length padding.
	 *
	 * @param prefix       the string prefix prepended to the numeric value (e.g. {@code "INV"})
	 * @param moduleName   the module name
	 * @param documentName the document name
	 * @param fieldName    the field name that holds the sequence
	 * @return the next sequence value string, e.g. {@code "INV42"}
	 */
	default String next(String prefix, String moduleName, String documentName, String fieldName) {
		return next(prefix, moduleName, documentName, fieldName, 0);
	}


	/**
	 * Returns a new document/sequence number for the given module.document.fieldName in a thread-safe way.
	 * <p>
	 * If no previous record is found in the DocumentNumber table, the method
	 * attempts to find the Maximum existing value currently extant in the field and
	 * increments that. Otherwise, the value returned is incremented and updated
	 * DocumentNumber value for the specified combination.
	 *
	 * @param prefix if the sequence value has a known prefix before the number, eg INV0001 has a prefix of "INV"
	 * @param moduleName the application module
	 * @param documentName the application document
	 * @param fieldName the fieldName/columnName in which the value is held
	 * @param minimumLength the minimum length of the number when specified as a string
	 * @return the next sequence number
	 */
	String next(String prefix,
					String moduleName,
					String documentName,
					String fieldName,
					int minimumLength);
}
