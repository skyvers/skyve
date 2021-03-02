package org.skyve.domain.types.converters.time;

import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.converters.Format;

public class HH24_MI extends AbstractTimeConverter {
	private static final String PATTERN = "HH:mm";

	@Override
	public Format<TimeOnly> getFormat() {
		return new Format<>("##:##", null);
	}

	@Override
	protected String getPattern() {
		return PATTERN;
	}
}
