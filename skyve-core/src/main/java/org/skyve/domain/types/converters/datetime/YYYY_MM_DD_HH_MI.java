package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.messages.ConversionException;

public class YYYY_MM_DD_HH_MI extends AbstractDateTimeConverter {
	public static final String PATTERN = "yyyy/MM/dd hh:mm a";

	@Override
	protected String getPattern() {
		return PATTERN;
	}

	@Override
	protected String getI18nKey() {
		return ConversionException.YYYY_MM_DD_HH_MI_KEY;
	}
}
