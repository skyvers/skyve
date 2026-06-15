package org.skyve.impl.domain.number;

import org.skyve.CORE;
import org.skyve.persistence.Persistence;

/**
 * Standard {@link AbstractDocumentNumberGenerator} that retrieves the current-thread
 * persistence instance from {@link org.skyve.CORE#getPersistence()} to increment
 * and return the next sequence value.
 *
 * <p>This is the default implementation registered via
 * {@link NumberGeneratorStaticSingleton#setDefault()}.
 *
 * @see AbstractDocumentNumberGenerator
 * @see NumberGeneratorStaticSingleton
 */
public class DocumentNumberGenerator extends AbstractDocumentNumberGenerator {
	@Override
	public String next(String prefix, String moduleName, String documentName, String fieldName, int minimumLength) {
		Persistence pers = CORE.getPersistence();
		return getNextNumber(pers, prefix, moduleName, documentName, fieldName, minimumLength);
	}
}
