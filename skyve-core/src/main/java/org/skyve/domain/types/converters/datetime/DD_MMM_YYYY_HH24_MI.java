package org.skyve.domain.types.converters.datetime;

public class DD_MMM_YYYY_HH24_MI extends AbstractDateTimeConverter {
	public static final String PATTERN = "dd-MMM-yyyy HH:mm";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
