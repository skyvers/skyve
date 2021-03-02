package org.skyve.domain.types.converters.datetime;

public class MMM_DD_YYYY extends AbstractDateTimeConverter {
	public static final String PATTERN = "MMM-dd-yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
