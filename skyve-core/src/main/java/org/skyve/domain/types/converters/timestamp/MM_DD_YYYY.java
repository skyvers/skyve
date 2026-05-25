package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link Timestamp} values using this class's configured
 * month-first date-only pattern.
 */
public class MM_DD_YYYY extends AbstractTimestampConverter {
	public static final String PATTERN = "MM/dd/yyyy";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.MM_DD_YYYY_TIMESTAMP_KEY;
	}
}
