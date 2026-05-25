package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link Timestamp} values using this class's configured
 * month-name, day-first, 24-hour timestamp pattern with seconds.
 */
public class MMM_DD_YYYY_HH24_MI_SS extends AbstractTimestampConverter {
	public static final String PATTERN = "MMM-dd-yyyy HH:mm:ss";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.MMM_DD_YYYY_HH24_MI_SS_KEY;
	}
}
