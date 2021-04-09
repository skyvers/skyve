package org.skyve.domain.types.converters.timestamp;

public class DD_MMM_YYYY extends AbstractTimestampConverter {
	private static final String PATTERN = "dd-MMM-yyyy";
	
	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
