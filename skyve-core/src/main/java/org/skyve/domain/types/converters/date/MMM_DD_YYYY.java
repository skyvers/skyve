package org.skyve.domain.types.converters.date;

public class MMM_DD_YYYY extends AbstractDateConverter {
	public static final String PATTERN = "MMM-dd-yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
