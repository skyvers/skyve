package org.skyve.domain.types.converters.timestamp;

public class DD_MM_YYYY_HH24_MI_SS extends AbstractTimestampConverter {
	private static final String PATTERN = "dd/MM/yyyy HH:mm:ss";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
