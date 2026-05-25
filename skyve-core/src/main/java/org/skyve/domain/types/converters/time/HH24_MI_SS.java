package org.skyve.domain.types.converters.time;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.converters.Format;

/**
 * Converts {@link TimeOnly} values using this class's configured
 * 24-hour time pattern with seconds.
 */
public class HH24_MI_SS extends AbstractTimeConverter {
	public static final String PATTERN = "HH:mm:ss";

	@Override
	public Format<TimeOnly> getFormat() {
		return new Format<>("##:##:##", null);
	}

	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	@Override
	protected String getI18nKey() {
		return ConversionException.HH24_MI_SS_KEY;
	}
}
