package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.messages.ConversionException;

public class DD_MM_YYYY_HH24_MI extends AbstractDateTimeConverter {
	public static final String PATTERN = "dd/MM/yyyy HH:mm";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}

	@Override
	protected String getI18nKey() {
		return ConversionException.DD_MM_YYYY_HH24_MI_KEY;
	}
}
