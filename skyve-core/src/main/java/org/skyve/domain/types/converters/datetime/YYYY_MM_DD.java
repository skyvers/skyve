package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link DateTime} values using this class's configured
 * year-first date-only pattern.
 */
@SuppressWarnings("java:S101") // Converter class names intentionally match metadata converter IDs.
public class YYYY_MM_DD extends AbstractDateTimeConverter {
	public static final String PATTERN = "yyyy/MM/dd";

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
		return ConversionException.YYYY_MM_DD_DATETIME_KEY;
	}
}
