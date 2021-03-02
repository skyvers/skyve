package org.skyve.domain.types.converters.timestamp;

public class DD_MMM_YYYY_HH24_MI_SS extends AbstractTimestampConverter {
	private static final String PATTERN = "dd-MMM-yyyy HH:mm:ss";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
