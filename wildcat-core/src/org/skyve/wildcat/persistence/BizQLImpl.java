package org.skyve.wildcat.persistence;

import org.skyve.domain.messages.DomainException;
import org.skyve.persistence.BizQL;

public final class BizQLImpl extends ProjectionQuery implements BizQL {
	private String query;
	private String resolvedQuery;

	public BizQLImpl(String query) {
		this.query = query;
	}

	@Override
	public String toQueryString() {
		return toQueryString(true);
	}

	String toQueryString(boolean checkForMalformation) {
		if (resolvedQuery == null) {
			try {
				resolveDocuments(checkForMalformation);
			}
			catch (Exception e) {
				throw new IllegalStateException("Could not resolve and secure query " + query, e);
			}
		}

		return resolvedQuery;
	}
	
	protected final void resolveDocuments(boolean checkForMalformation) throws DomainException {
		AbstractPersistence persistence = AbstractPersistence.get();

		StringBuilder result = new StringBuilder(query);
		
		int openBraceIndex = result.indexOf("{");
		if (checkForMalformation && (openBraceIndex < 0)) {
			throw new DomainException("Malformed select statement - no opening curly brace to delimit a document.  Use 'select bean from {admin.User} as bean'");
		}
		int closeBraceIndex = result.indexOf("}");

		while (openBraceIndex >= 0) {
			if (closeBraceIndex < 0) {
				throw new DomainException("Malformed select statement - no closing curly brace to delimit a document.  Use 'select bean from {admin.User} as bean'");
			}

			String moduleDotDocument = result.substring(openBraceIndex + 1, closeBraceIndex);
			int dotIndex = moduleDotDocument.indexOf('.');
			if (dotIndex < 0) {
				throw new DomainException("A document needs to be of the form <module>.<document>");
			}
			String moduleName = moduleDotDocument.substring(0, dotIndex);
			if (drivingModuleName == null) {
				drivingModuleName = moduleName;
			}
			String documentName = moduleDotDocument.substring(dotIndex + 1);
			if (drivingDocumentName == null) {
				drivingDocumentName = documentName;
			}
			result.replace(openBraceIndex, closeBraceIndex + 1, persistence.getDocumentEntityName(moduleName, documentName));

			openBraceIndex = result.indexOf("{");
			closeBraceIndex = result.indexOf("}");
		}

		resolvedQuery = result.toString();
	}
}
