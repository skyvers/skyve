package org.skyve.persistence;

import java.util.Map;

/**
 * 
 */
public interface Query {
	/**
	 * 
	 * @return
	 */
	public Map<String, Object> getParameters();
	
	/**
	 * 
	 * @param name
	 * @param value
	 */
	public void putParameter(String name, Object value);
}
