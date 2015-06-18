package org.skyve.persistence;

import java.util.Map;

/**
 * 
 */
public interface Query {
	public Map<String, Object> getParameters();
	public Query putParameter(String name, Object value);
}
