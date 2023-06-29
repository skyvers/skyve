package org.skyve.domain.types.converters.time;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.converters.Format;

public class HH24_MI extends AbstractTimeConverter {
	public static final String PATTERN = "HH:mm";

	@Override
	public Format<TimeOnly> getFormat() {
		return new Format<>("##:##", null);
	}

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.HH24_MI_KEY;
	}
}
