package org.skyve.domain.types.converters.datetime;

public class DD_MMM_YYYY extends AbstractDateTimeConverter {
	public static final String PATTERN = "dd-MMM-yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
