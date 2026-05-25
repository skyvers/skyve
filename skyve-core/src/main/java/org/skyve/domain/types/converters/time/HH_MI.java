package org.skyve.domain.types.converters.time;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link TimeOnly} values using this class's configured
 * 12-hour time pattern without seconds.
 */
public class HH_MI extends AbstractTimeConverter {
	public static final String PATTERN = "hh:mm a";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.HH_MI_KEY;
	}
}
