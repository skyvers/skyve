package org.skyve.domain.types.converters.time;

import org.skyve.domain.messages.ConversionException;

public class HH_MI_SS extends AbstractTimeConverter {
	private static final String PATTERN = "hh:mm:ss a";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.HH_MI_SS_KEY;
	}
}
