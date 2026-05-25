package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link Timestamp} values using this class's configured
 * day-first, month-name date-only pattern.
 */
public class DD_MMM_YYYY extends AbstractTimestampConverter {
	private static final String PATTERN = "dd-MMM-yyyy";
	
	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.DD_MMM_YYYY_TIMESTAMP_KEY;
	}
}
