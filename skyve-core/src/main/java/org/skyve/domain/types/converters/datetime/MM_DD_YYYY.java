package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.messages.ConversionException;

public class MM_DD_YYYY extends AbstractDateTimeConverter {
	public static final String PATTERN = "MM/dd/yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}

	@Override
	protected String getI18nKey() {
		return ConversionException.MM_DD_YYYY_DATETIME_KEY;
	}
}
