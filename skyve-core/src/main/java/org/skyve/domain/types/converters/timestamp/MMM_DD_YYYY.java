package org.skyve.domain.types.converters.timestamp;

public class MMM_DD_YYYY extends AbstractTimestampConverter {
	private static final String PATTERN = "MMM-dd-yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
