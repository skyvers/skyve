package org.skyve.domain.types.formatters;

import java.util.Date;

import org.skyve.CORE;

/**
 * Format a java.util.Date into a temporal representation. 
 */
public class SimpleDateFormatter implements Formatter<Date> {
	private String simpleDateFormatPattern;
	
	/**
	 * Creates a new SimpleDateFormatter instance.
	 * @param simpleDateFormatPattern the simpleDateFormatPattern
	 */
	public SimpleDateFormatter(String simpleDateFormatPattern) {
		this.simpleDateFormatPattern = simpleDateFormatPattern;
	}
	
	/**
	 * Returns the valueType.
	 * @return the result
	 */
	@Override
	public Class<Date> getValueType() {
		return Date.class;
	}
	
	/**
	 * Executes toDisplayValue.
	 * @param value the value
	 * @return the result
	 */
	@Override
	public String toDisplayValue(Date value) {
		return CORE.getDateFormat(simpleDateFormatPattern).format(value);
	}
}
