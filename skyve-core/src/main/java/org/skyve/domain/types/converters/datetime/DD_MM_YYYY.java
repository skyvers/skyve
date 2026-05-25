package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link DateTime} values using this class's configured
 * day-first date-only pattern.
 */
public class DD_MM_YYYY extends AbstractDateTimeConverter {
	public static final String PATTERN = "dd/MM/yyyy";

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
		return ConversionException.DD_MM_YYYY_DATETIME_KEY;
	}
}
