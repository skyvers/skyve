package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.messages.ConversionException;

public class YYYY_MM_DD extends AbstractDateTimeConverter {
	public static final String PATTERN = "yyyy/MM/dd";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}

	@Override
	protected String getI18nKey() {
		return ConversionException.YYYY_MM_DD_DATETIME_KEY;
	}
}
