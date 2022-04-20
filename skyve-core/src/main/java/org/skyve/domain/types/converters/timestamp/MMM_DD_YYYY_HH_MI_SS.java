package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

public class MMM_DD_YYYY_HH_MI_SS extends AbstractTimestampConverter {
	public static final String PATTERN = "MMM-dd-yyyy hh:mm:ss a";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.MMM_DD_YYYY_HH_MI_SS_KEY;
	}
}
