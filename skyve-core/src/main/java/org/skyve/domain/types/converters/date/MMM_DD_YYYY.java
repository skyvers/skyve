package org.skyve.domain.types.converters.date;

import org.skyve.domain.messages.ConversionException;

public class MMM_DD_YYYY extends AbstractDateConverter {
	public static final String PATTERN = "MMM-dd-yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.MMM_DD_YYYY_KEY;
	}
}
