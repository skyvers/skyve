package org.skyve.impl.domain.number;

import org.skyve.CORE;
import org.skyve.persistence.Persistence;

public class DocumentNumberGenerator extends AbstractDocumentNumberGenerator {

	@Override
	public String next(String prefix, String moduleName, String documentName, String fieldName, int numberLength) throws Exception {

		Persistence pers = CORE.getPersistence();

		return getNextNumber(pers, prefix, moduleName, documentName, fieldName, numberLength);
	}
}
