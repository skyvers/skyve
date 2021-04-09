package org.skyve.domain.types.converters.date;

public class DD_MM_YYYY extends AbstractDateConverter {
	public static final String PATTERN = "dd/MM/yyyy";

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
