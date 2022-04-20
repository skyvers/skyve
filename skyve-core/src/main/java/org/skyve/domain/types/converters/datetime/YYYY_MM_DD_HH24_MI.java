package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.messages.ConversionException;

public class YYYY_MM_DD_HH24_MI extends AbstractDateTimeConverter {
	public static final String PATTERN = "yyyy/MM/dd HH:mm";

	@Override
	protected String getPattern() {
		return PATTERN;
	}

	@Override
	protected String getI18nKey() {
		return ConversionException.YYYY_MM_DD_HH24_MI_KEY;
	}
}
