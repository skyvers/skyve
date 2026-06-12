package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link Timestamp} values using this class's configured
 * month-first date-only pattern.
 */
@SuppressWarnings("java:S101") // Converter class names intentionally match metadata converter IDs.
public class MM_DD_YYYY extends AbstractTimestampConverter {
	public static final String PATTERN = "MM/dd/yyyy";

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
		return ConversionException.MM_DD_YYYY_TIMESTAMP_KEY;
	}
}
