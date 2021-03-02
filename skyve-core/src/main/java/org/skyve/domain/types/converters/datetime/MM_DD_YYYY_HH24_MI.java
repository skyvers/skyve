package org.skyve.domain.types.converters.datetime;

public class MM_DD_YYYY_HH24_MI extends AbstractDateTimeConverter {
	public static final String PATTERN = "MM/dd/yyyy HH:mm";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
