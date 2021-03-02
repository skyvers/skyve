package org.skyve.domain.types.converters.timestamp;

public class MMM_DD_YYYY_HH_MI_SS extends AbstractTimestampConverter {
	public static final String PATTERN = "MMM-dd-yyyy hh:mm:ss a";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
