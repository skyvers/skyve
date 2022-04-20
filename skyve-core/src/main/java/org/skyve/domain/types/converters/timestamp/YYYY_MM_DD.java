package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

public class YYYY_MM_DD extends AbstractTimestampConverter {
	private static final String PATTERN = "yyyy/MM/dd";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.YYYY_MM_DD_TIMESTAMP_KEY;
	}
}
