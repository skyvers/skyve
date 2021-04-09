package org.skyve.domain.types.converters.datetime;

public class DD_MM_YYYY extends AbstractDateTimeConverter {
	public static final String PATTERN = "dd/MM/yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
