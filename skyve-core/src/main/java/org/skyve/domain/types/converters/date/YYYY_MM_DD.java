package org.skyve.domain.types.converters.date;

import org.skyve.domain.messages.ConversionException;

public class YYYY_MM_DD extends AbstractDateConverter {
	public static final String PATTERN = "yyyy/MM/dd";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.YYYY_MM_DD_KEY;
	}
}
