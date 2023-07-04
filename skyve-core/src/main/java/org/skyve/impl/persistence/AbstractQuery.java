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

	// 0 = default timeout (for oltp or async threads), > 0 sets a timeout, < 0 means no timeout
	protected int timeoutInSeconds = 0; // default timeout

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

	/**
	 * Returns exactly one result from this query; or throws a NoResultsException or 
	 * ManyResultsException based on the number of results.
	 * 
	 * @param results The complete list of results
	 * @return The single result from the list, or an exception if the list does not contain one result
	 */
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
	
	/**
	 * Returns the first result from this query, or null if there are no
	 * results.
	 * 
	 * @param results The list of all results
	 * @return The first result, or null
	 */
	public static <T> T returnOneResult(List<T> results) {
		if (results.isEmpty()) {
			return null;
		}
		return results.get(0);
	}
	
	public int getTimeoutInSeconds() {
		return timeoutInSeconds;
	}
	public void setTimeoutInSeconds(int timeoutInSeconds) {
		this.timeoutInSeconds = timeoutInSeconds;
	}
}
