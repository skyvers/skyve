package org.skyve.wildcat.persistence;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.persistence.Query;

abstract class QueryImpl implements Query {
	private Map<String, Object> parameters = new TreeMap<>();
	protected int parameterNumber = 0; // keep parameter names unique if created programatically

	@Override
	public final Map<String, Object> getParameters() {
		return parameters;
	}

	@Override
	public void putParameter(String name, Object value) {
		parameters.put(name, value);
	}

	public abstract String toQueryString();
}
