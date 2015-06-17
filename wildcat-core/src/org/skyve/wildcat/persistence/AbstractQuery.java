package org.skyve.wildcat.persistence;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.ManyResultsException;
import org.skyve.domain.messages.NoResultsException;
import org.skyve.persistence.Query;

public abstract class AbstractQuery implements Query {
	private Map<String, Object> parameters = new TreeMap<>();
	protected int parameterNumber = 0; // keep parameter names unique if created programatically

	protected String drivingModuleName;
	protected String drivingDocumentName;

	@Override
	public final Map<String, Object> getParameters() {
		return parameters;
	}

	@Override
	public Query putParameter(String name, Object value) {
		parameters.put(name, value);
		return this;
	}

	public String getDrivingModuleName() {
		return drivingModuleName;
	}

	public String getDrivingDocumentName() {
		return drivingDocumentName;
	}

	public abstract String toQueryString();

	@Override
	public final <T extends Bean> T beanResult()
	throws DomainException {
		List<T> results = beanResults();
		if (results.size() == 1) {
			return results.get(0);
		}
		else if (results.isEmpty()) {
			throw new NoResultsException();
		}
		else {
			throw new ManyResultsException();
		}
	}

	@Override
	public final <T extends Bean> T projectedResult()
	throws DomainException {
		List<T> results = projectedResults();
		if (results.size() == 1) {
			return results.get(0);
		}
		else if (results.isEmpty()) {
			throw new NoResultsException();
		}
		else {
			throw new ManyResultsException();
		}
	}

	@Override
	public final <T> T scalarResult(Class<T> type)
	throws DomainException {
		List<T> results = scalarResults(type);
		if (results.size() == 1) {
			return results.get(0);
		}
		else if (results.isEmpty()) {
			throw new NoResultsException();
		}
		else {
			throw new ManyResultsException();
		}
	}

	@Override
	public final Object[] tupleResult()
	throws DomainException {
		List<Object[]> results = tupleResults();
		if (results.size() == 1) {
			return results.get(0);
		}
		else if (results.isEmpty()) {
			throw new NoResultsException();
		}
		else {
			throw new ManyResultsException();
		}
	}
}
