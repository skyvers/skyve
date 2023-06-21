package org.skyve.domain.types.converters.time;

import org.skyve.domain.messages.ConversionException;

public class HH_MI extends AbstractTimeConverter {
	public static final String PATTERN = "hh:mm a";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.HH_MI_KEY;
	}
}
