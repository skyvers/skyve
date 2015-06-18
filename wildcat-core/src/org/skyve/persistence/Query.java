package org.skyve.persistence;

import java.util.List;
import java.util.Map;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;

/**
 * 
 */
public interface Query {
	public Map<String, Object> getParameters();
	public Query putParameter(String name, Object value);
}
