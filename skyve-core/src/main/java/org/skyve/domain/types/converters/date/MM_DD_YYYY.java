package org.skyve.domain.types.converters.date;

import org.skyve.domain.messages.ConversionException;

public class MM_DD_YYYY extends AbstractDateConverter {
	public static final String PATTERN = "MM/dd/yyyy";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.MM_DD_YYYY_KEY;
	}
}
