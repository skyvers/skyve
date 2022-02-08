package org.skyve.domain.number;

public interface NumberGenerator {
	/**
	 * Next Integer.
	 */
	default Integer nextInt(String moduleName, String documentName, String fieldName) {
		return Integer.valueOf(Integer.parseInt(next(null, moduleName, documentName, fieldName, 0)));
	}

	/**
	 * Next Long.
	 */
	default Long nextLong(String moduleName, String documentName, String fieldName) {
		return Long.valueOf(Long.parseLong(next(null, moduleName, documentName, fieldName, 0)));
	}

	/**
	 * Returns a new document/sequence number for the given
	 * module.document.fieldName in a thread-safe way.
	 * <p>
	 * If no previous record is found in the DocumentNumber table, the method
	 * attempts to find the Maximum existing value currently extant in the field and
	 * increments that. Otherwise, the value returned is incremented and updated
	 * DocumentNumber value for the specified combination.
	 *
	 * @param prefix	if the sequence value has a known prefix before the number, eg INV0001 has a prefix of "INV"
	 * @param moduleName	the application module
	 * @param documentName	the application document
	 * @param fieldName	the fieldName/columnName in which the value is held
	 * @param numberLength	the minimum length of the number when specified as a string
	 * @return	the next sequence number
	 */
	String next(String prefix,
					String moduleName,
					String documentName,
					String fieldName,
					int numberLength);
}
