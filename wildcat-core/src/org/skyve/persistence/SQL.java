package org.skyve.persistence;

/**
 * 
 */
public interface SQL extends Query, BeanQuery, ScalarQuery, TupleQuery, DMLQuery {
	@Override
	public SQL putParameter(String name, Object value);
}
