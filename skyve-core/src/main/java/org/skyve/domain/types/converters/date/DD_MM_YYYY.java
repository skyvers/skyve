package org.skyve.domain.types.converters.date;

import org.skyve.domain.messages.ConversionException;

public class DD_MM_YYYY extends AbstractDateConverter {
	public static final String PATTERN = "dd/MM/yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.DD_MM_YYYY_KEY;
	}
}
