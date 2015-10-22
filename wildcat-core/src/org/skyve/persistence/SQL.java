package org.skyve.persistence;

import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.model.Attribute.AttributeType;

import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 */
public interface SQL extends BeanQuery, ScalarQuery, TupleQuery, DMLQuery {
	public SQL putParameter(String name, DateOnly value);
	public SQL putParameter(String name, DateTime value);
	public SQL putParameter(String name, TimeOnly value);
	public SQL putParameter(String name, Timestamp value);
	public SQL putParameter(String name, Decimal value);
	public SQL putParameter(String name, Integer value);
	public SQL putParameter(String name, Long value);
	public SQL putParameter(String name, String value, boolean memoOrMarkup);
	public SQL putParameter(String name, Bean value);
	public SQL putParameter(String name, Geometry value);
	public SQL putParameter(String name, Boolean value);
	public SQL putParameter(String name, Enumeration value);
	public SQL putParameter(String name, Object value, AttributeType type);
}
