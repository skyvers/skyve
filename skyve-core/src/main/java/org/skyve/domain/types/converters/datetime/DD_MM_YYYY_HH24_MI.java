package org.skyve.domain.types.converters.datetime;

public class DD_MM_YYYY_HH24_MI extends AbstractDateTimeConverter {
	public static final String PATTERN = "dd/MM/yyyy HH:mm";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
