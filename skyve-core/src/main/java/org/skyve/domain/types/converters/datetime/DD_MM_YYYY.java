package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.messages.ConversionException;

public class DD_MM_YYYY extends AbstractDateTimeConverter {
	public static final String PATTERN = "dd/MM/yyyy";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}

	@Override
	protected String getI18nKey() {
		return ConversionException.DD_MM_YYYY_DATETIME_KEY;
	}
}
