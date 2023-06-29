package org.skyve.domain.types.converters.time;

import org.skyve.domain.messages.ConversionException;

public class HH_MI_SS extends AbstractTimeConverter {
	public static final String PATTERN = "hh:mm:ss a";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.HH_MI_SS_KEY;
	}
}
