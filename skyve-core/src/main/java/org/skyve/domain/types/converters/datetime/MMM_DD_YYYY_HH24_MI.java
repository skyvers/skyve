package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.messages.ConversionException;

public class MMM_DD_YYYY_HH24_MI extends AbstractDateTimeConverter {
	public static final String PATTERN = "MMM-dd-yyyy HH:mm";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}

	@Override
	protected String getI18nKey() {
		return ConversionException.MMM_DD_YYYY_HH24_MI_KEY;
	}
}
