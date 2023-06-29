package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.messages.ConversionException;

public class MMM_DD_YYYY_HH_MI extends AbstractDateTimeConverter {
	public static final String PATTERN = "MMM-dd-yyyy hh:mm a";

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}

	@Override
	protected String getI18nKey() {
		return ConversionException.MMM_DD_YYYY_HH_MI_KEY;
	}
}
