package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

import jakarta.annotation.Nonnull;

/**
 * Converts {@link Timestamp} values using this class's configured
 * year-first, 12-hour timestamp pattern with seconds.
 */
@SuppressWarnings("java:S101") // Converter class names intentionally match metadata converter IDs.
public class YYYY_MM_DD_HH_MI_SS extends AbstractTimestampConverter {
	public static final String PATTERN = "yyyy/MM/dd hh:mm:ss a";

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
		return ConversionException.YYYY_MM_DD_HH_MI_SS_KEY;
	}
}
