package org.skyve.domain.types.converters.timestamp;

public class DD_MM_YYYY_HH_MI_SS extends AbstractTimestampConverter {
	private static final String PATTERN = "dd/MM/yyyy hh:mm:ss a";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
