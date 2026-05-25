package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link Timestamp} values using this class's configured
 * month-first, 12-hour timestamp pattern with seconds.
 */
public class MM_DD_YYYY_HH_MI_SS extends AbstractTimestampConverter {
	public static final String PATTERN = "MM/dd/yyyy hh:mm:ss a";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.MM_DD_YYYY_HH_MI_SS_KEY;
	}
}
