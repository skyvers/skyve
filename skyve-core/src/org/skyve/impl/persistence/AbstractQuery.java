package org.skyve.impl.persistence;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.skyve.domain.messages.ManyResultsException;
import org.skyve.domain.messages.NoResultsException;

public abstract class AbstractQuery {
	protected Map<String, Object> parameters = new TreeMap<>();
	
	protected int parameterNumber = 0; // keep parameter names unique if created programmatically

	protected String drivingModuleName;
	protected String drivingDocumentName;

	public final Set<String> getParameterNames() {
		return parameters.keySet();
	}

	public final Object getParameter(String name) {
		return parameters.get(name);
	}
	
	public String getDrivingModuleName() {
		return drivingModuleName;
	}

	public String getDrivingDocumentName() {
		return drivingDocumentName;
	}

	public abstract String toQueryString();

	public static <T> T assertOneResult(List<T> results) {
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
	
	public static <T> T returnOneResult(List<T> results) {
		if (results.isEmpty()) {
			return null;
		}
		return results.get(0);
	}
}
