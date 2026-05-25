package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link DateTime} values using this class's configured
 * month-first, 12-hour date-time pattern.
 */
public class MM_DD_YYYY_HH_MI extends AbstractDateTimeConverter {
	public static final String PATTERN = "MM/dd/yyyy hh:mm a";

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
		return ConversionException.MM_DD_YYYY_HH_MI_KEY;
	}
}
