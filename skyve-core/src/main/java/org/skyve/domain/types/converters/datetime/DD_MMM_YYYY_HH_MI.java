package org.skyve.domain.types.converters.datetime;

public class DD_MMM_YYYY_HH_MI extends AbstractDateTimeConverter {
	public static final String PATTERN = "dd-MMM-yyyy hh:mm a";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
