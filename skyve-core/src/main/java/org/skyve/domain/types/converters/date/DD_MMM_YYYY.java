package org.skyve.domain.types.converters.date;

public class DD_MMM_YYYY extends AbstractDateConverter {
	public static final String PATTERN = "dd-MMM-yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
