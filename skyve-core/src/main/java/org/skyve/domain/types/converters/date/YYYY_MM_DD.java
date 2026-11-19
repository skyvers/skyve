package org.skyve.domain.types.converters.date;

import org.skyve.domain.messages.ConversionException;

import jakarta.annotation.Nonnull;

/**
 * Converts {@link org.skyve.domain.types.DateOnly} values using
 * {@code yyyy/MM/dd} formatting.
 */
@SuppressWarnings("java:S101") // Converter class names intentionally match metadata converter IDs.
public class YYYY_MM_DD extends AbstractDateConverter {
	public static final String PATTERN = "yyyy/MM/dd";

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
		return ConversionException.YYYY_MM_DD_KEY;
	}
}
