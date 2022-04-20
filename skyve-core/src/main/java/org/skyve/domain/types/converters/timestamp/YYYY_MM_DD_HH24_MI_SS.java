package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

public class YYYY_MM_DD_HH24_MI_SS extends AbstractTimestampConverter {
	public static final String PATTERN = "yyyy/MM/dd HH:mm:ss";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.YYYY_MM_DD_HH24_MI_SS_KEY;
	}
}
