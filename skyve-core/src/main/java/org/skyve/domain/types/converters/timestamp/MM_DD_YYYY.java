package org.skyve.domain.types.converters.timestamp;

public class MM_DD_YYYY extends AbstractTimestampConverter {
	public static final String PATTERN = "MM/dd/yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
