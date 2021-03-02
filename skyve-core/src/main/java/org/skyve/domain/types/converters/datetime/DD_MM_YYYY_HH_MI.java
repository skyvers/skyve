package org.skyve.domain.types.converters.datetime;

public class DD_MM_YYYY_HH_MI extends AbstractDateTimeConverter {
	public static final String PATTERN = "dd/MM/yyyy hh:mm a";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
