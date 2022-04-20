package org.skyve.domain.types.converters.date;

import org.skyve.domain.messages.ConversionException;

public class DD_MMM_YYYY extends AbstractDateConverter {
	public static final String PATTERN = "dd-MMM-yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.DD_MMM_YYYY_KEY;
	}
}
