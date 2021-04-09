package org.skyve.domain.types.converters.datetime;

public class MM_DD_YYYY_HH_MI extends AbstractDateTimeConverter {
	public static final String PATTERN = "MM/dd/yyyy hh:mm a";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
