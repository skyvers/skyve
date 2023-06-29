package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

public class DD_MM_YYYY_HH_MI_SS extends AbstractTimestampConverter {
	public static final String PATTERN = "dd/MM/yyyy hh:mm:ss a";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.DD_MM_YYYY_HH_MI_SS_KEY;
	}
}
