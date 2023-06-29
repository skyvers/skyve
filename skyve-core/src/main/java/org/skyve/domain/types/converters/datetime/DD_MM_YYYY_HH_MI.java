package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.messages.ConversionException;

public class DD_MM_YYYY_HH_MI extends AbstractDateTimeConverter {
	public static final String PATTERN = "dd/MM/yyyy hh:mm a";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.DD_MM_YYYY_HH_MI_KEY;
	}
}
