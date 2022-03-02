package org.skyve.impl.persistence;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.BizQL;

public class AbstractBizQL extends AbstractQuery implements BizQL {
	private String query;
	private String resolvedQuery;

	public AbstractBizQL(String query) {
		this.query = query;
	}

	@Override
	public AbstractBizQL noTimeout() {
		this.timeoutInSeconds = Integer.MIN_VALUE;
		return this;
	}
	
	@Override
	public AbstractBizQL putParameter(String name, Object value) {
		parameters.put(name, value);
		if (UtilImpl.QUERY_TRACE) {
			UtilImpl.LOGGER.info("    SET PARAM " + name + " = " + value);
		}
		return this;
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
	
	protected final void resolveDocuments(boolean checkForMalformation) {
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

	@Override
	public <T extends Bean> List<T> beanResults() {
		// No-op
		return null;
	}

	@Override
	public final <T extends Bean> T retrieveBean() {
		List<T> results = beanResults();
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final <T extends Bean> T beanResult() {
		List<T> results = beanResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> beanIterable() {
		// No-op
		return null;
	}

	@Override
	public <T extends Bean> List<T> projectedResults() {
		// No-op
		return null;
	}

	@Override
	public final <T extends Bean> T projectedResult() {
		List<T> results = projectedResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public final <T extends Bean> T retrieveProjected() {
		List<T> results = projectedResults();
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public <T extends Bean> AutoClosingIterable<T> projectedIterable() {
		// No-op
		return null;
	}

	@Override
	public <T> List<T> scalarResults(Class<T> type) {
		// No-op
		return null;
	}

	@Override
	public final <T> T retrieveScalar(Class<T> type) {
		List<T> results = scalarResults(type);
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final <T> T scalarResult(Class<T> type) {
		List<T> results = scalarResults(type);
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public <T> AutoClosingIterable<T> scalarIterable(Class<T> type) {
		// No-op
		return null;
	}

	@Override
	public List<Object[]> tupleResults() {
		// No-op
		return null;
	}

	@Override
	public final Object[] retrieveTuple() {
		List<Object[]> results = tupleResults();
		return AbstractQuery.assertOneResult(results);
	}

	@Override
	public final Object[] tupleResult() {
		List<Object[]> results = tupleResults();
		return AbstractQuery.returnOneResult(results);
	}

	@Override
	public AutoClosingIterable<Object[]> tupleIterable() {
		// No-op
		return null;
	}

	@Override
	public int execute() {
		// No-op
		return 0;
	}

	@Override
	public BizQL setFirstResult(int first) {
		// No-op
		return null;
	}

	@Override
	public BizQL setMaxResults(int max) {
		// No-op
		return null;
	}
}
