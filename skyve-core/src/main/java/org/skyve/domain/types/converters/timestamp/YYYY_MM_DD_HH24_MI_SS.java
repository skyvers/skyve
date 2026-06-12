package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link Timestamp} values using this class's configured
 * year-first, 24-hour timestamp pattern with seconds.
 */
@SuppressWarnings("java:S101") // Converter class names intentionally match metadata converter IDs.
public class YYYY_MM_DD_HH24_MI_SS extends AbstractTimestampConverter {
	public static final String PATTERN = "yyyy/MM/dd HH:mm:ss";

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
		return ConversionException.YYYY_MM_DD_HH24_MI_SS_KEY;
	}
}
