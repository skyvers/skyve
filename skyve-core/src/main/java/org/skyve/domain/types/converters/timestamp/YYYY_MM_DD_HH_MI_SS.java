package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

public class YYYY_MM_DD_HH_MI_SS extends AbstractTimestampConverter {
	public static final String PATTERN = "yyyy/MM/dd hh:mm:ss a";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.YYYY_MM_DD_HH_MI_SS_KEY;
	}
}
