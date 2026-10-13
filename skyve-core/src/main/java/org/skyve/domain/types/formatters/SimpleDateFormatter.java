package org.skyve.domain.types.formatters;

import java.util.Date;

import org.skyve.CORE;

import jakarta.annotation.Nonnull;

/**
 * Format a java.util.Date into a temporal representation. 
 */
public class SimpleDateFormatter implements Formatter<Date> {
	private String simpleDateFormatPattern;
	
	/**
	 * Creates a new SimpleDateFormatter instance.
	 * @param simpleDateFormatPattern the simpleDateFormatPattern
	 */
	public SimpleDateFormatter(@Nonnull String simpleDateFormatPattern) {
		this.simpleDateFormatPattern = simpleDateFormatPattern;
	}
	
	/**
	 * Returns the valueType.
	 * @return the result
	 */
	@Override
	public @Nonnull Class<Date> getValueType() {
		return Date.class;
	}
	
	/**
	 * Executes toDisplayValue.
	 * @param value the value
	 * @return the result
	 */
	@Override
	public @Nonnull String toDisplayValue(@Nonnull Date value) {
		return CORE.getDateFormat(simpleDateFormatPattern).format(value);
	}
}
