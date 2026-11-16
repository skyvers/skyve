package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.messages.ConversionException;

import jakarta.annotation.Nonnull;

/**
 * Converts {@link DateTime} values using this class's configured
 * day-first, 12-hour date-time pattern.
 */
@SuppressWarnings("java:S101") // Converter class names intentionally match metadata converter IDs.
public class DD_MM_YYYY_HH_MI extends AbstractDateTimeConverter {
	public static final String PATTERN = "dd/MM/yyyy hh:mm a";

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
		return ConversionException.DD_MM_YYYY_HH_MI_KEY;
	}
}
