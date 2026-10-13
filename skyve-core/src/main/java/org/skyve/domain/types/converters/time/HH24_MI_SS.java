package org.skyve.domain.types.converters.time;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.converters.Format;

import jakarta.annotation.Nonnull;

/**
 * Converts {@link TimeOnly} values using this class's configured
 * 24-hour time pattern with seconds.
 */
@SuppressWarnings("java:S101") // Converter class names intentionally match metadata converter IDs.
public class HH24_MI_SS extends AbstractTimeConverter {
	public static final String PATTERN = "HH:mm:ss";

	/**
	 * Returns the optional format descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public @Nonnull Format<TimeOnly> getFormat() {
		return new Format<>("##:##:##", null);
	}

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
		return ConversionException.HH24_MI_SS_KEY;
	}
}
