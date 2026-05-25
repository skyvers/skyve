package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link Timestamp} values using this class's configured
 * year-first, 12-hour timestamp pattern with seconds.
 */
public class YYYY_MM_DD_HH_MI_SS extends AbstractTimestampConverter {
	public static final String PATTERN = "yyyy/MM/dd hh:mm:ss a";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.YYYY_MM_DD_HH_MI_SS_KEY;
	}
}
