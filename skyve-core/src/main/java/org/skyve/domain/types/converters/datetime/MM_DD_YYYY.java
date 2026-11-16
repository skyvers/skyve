package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.messages.ConversionException;

import jakarta.annotation.Nonnull;

/**
 * Converts {@link DateTime} values using this class's configured
 * month-first date-only pattern.
 */
@SuppressWarnings("java:S101") // Converter class names intentionally match metadata converter IDs.
public class MM_DD_YYYY extends AbstractDateTimeConverter {
	public static final String PATTERN = "MM/dd/yyyy";

	/**
	 * Returns the optional format pattern for this converter.
	 * @return the result value
	 */
	@Override
	public @Nonnull String getFormatPattern() {
		return PATTERN;
	}

	/**
	 * Returns the i18n key.
	 * @return the result value
	 */
	@Override
	protected @Nonnull String getI18nKey() {
		return ConversionException.MM_DD_YYYY_DATETIME_KEY;
	}
}
