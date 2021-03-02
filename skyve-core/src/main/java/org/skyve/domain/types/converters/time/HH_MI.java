package org.skyve.domain.types.converters.time;

public class HH_MI extends AbstractTimeConverter {
	private static final String PATTERN = "hh:mm a";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
