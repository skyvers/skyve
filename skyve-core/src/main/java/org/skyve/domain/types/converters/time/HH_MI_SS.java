package org.skyve.domain.types.converters.time;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link TimeOnly} values using this class's configured
 * 12-hour time pattern with seconds.
 */
public class HH_MI_SS extends AbstractTimeConverter {
	public static final String PATTERN = "hh:mm:ss a";

	/**
	 * Returns the optional format pattern for this converter.
	 * @return the result value
	 */
	@Override
	public String getFormatPattern() {
		return PATTERN;
	}
	
	/**
	 * Returns the i18n key.
	 * @return the result value
	 */
	@Override
	protected String getI18nKey() {
		return ConversionException.HH_MI_SS_KEY;
	}
}
