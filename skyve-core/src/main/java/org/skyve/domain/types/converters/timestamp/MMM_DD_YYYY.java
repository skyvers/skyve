package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

public class MMM_DD_YYYY extends AbstractTimestampConverter {
	private static final String PATTERN = "MMM-dd-yyyy";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.MMM_DD_YYYY_TIMESTAMP_KEY;
	}
}
