package org.skyve.domain.types.converters.time;

public class HH_MI_SS extends AbstractTimeConverter {
	private static final String PATTERN = "hh:mm:ss a";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
