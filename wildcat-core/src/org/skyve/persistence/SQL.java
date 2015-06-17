package org.skyve.persistence;

/**
 * 
 */
public interface SQL extends Query, DMLQuery {
	@Override
	public SQL putParameter(String name, Object value);
}
