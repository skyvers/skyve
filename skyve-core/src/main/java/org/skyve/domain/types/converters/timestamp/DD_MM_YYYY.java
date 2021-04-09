package org.skyve.domain.types.converters.timestamp;

public class DD_MM_YYYY extends AbstractTimestampConverter {
	private static final String PATTERN = "dd/MM/yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
