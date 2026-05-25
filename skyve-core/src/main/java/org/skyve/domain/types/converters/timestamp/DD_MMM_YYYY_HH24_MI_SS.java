package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link Timestamp} values using this class's configured
 * day-first, month-name, 24-hour timestamp pattern with seconds.
 */
public class DD_MMM_YYYY_HH24_MI_SS extends AbstractTimestampConverter {
	public static final String PATTERN = "dd-MMM-yyyy HH:mm:ss";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.DD_MMM_YYYY_HH24_MI_SS_KEY;
	}
}
