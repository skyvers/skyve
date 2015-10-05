package org.skyve.persistence;

import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * 
 */
public interface SQL extends Query, BeanQuery, ScalarQuery, TupleQuery, DMLQuery {
	@Override
	public SQL putParameter(String name, Object value);
	public SQL putParameter(String name, Object value, AttributeType type);
}
