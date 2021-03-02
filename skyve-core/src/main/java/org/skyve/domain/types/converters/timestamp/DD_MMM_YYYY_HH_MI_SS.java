package org.skyve.domain.types.converters.timestamp;

public class DD_MMM_YYYY_HH_MI_SS extends AbstractTimestampConverter {
	private static final String PATTERN = "dd-MMM-yyyy hh:mm:ss a";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
