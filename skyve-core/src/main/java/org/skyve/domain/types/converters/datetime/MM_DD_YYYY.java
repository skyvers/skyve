package org.skyve.domain.types.converters.datetime;

public class MM_DD_YYYY extends AbstractDateTimeConverter {
	public static final String PATTERN = "MM/dd/yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
