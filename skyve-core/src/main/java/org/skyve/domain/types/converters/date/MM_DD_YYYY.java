package org.skyve.domain.types.converters.date;

public class MM_DD_YYYY extends AbstractDateConverter {
	public static final String PATTERN = "MM/dd/yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
