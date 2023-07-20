package org.skyve.domain.types.formatters;

import java.util.Date;

import org.skyve.CORE;

/**
 * Format a java.util.Date into a temporal representation. 
 */
public class SimpleDateFormatter implements Formatter<Date> {
	private String simpleDateFormatPattern;
	
	public SimpleDateFormatter(String simpleDateFormatPattern) {
		this.simpleDateFormatPattern = simpleDateFormatPattern;
	}
	
	@Override
	public Class<Date> getValueType() {
		return Date.class;
	}
	
	@Override
	public String toDisplayValue(Date value) {
		return CORE.getDateFormat(simpleDateFormatPattern).format(value);
	}
}
