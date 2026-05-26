package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link Timestamp} values using this class's configured
 * day-first, month-name, 12-hour timestamp pattern with seconds.
 */
public class DD_MMM_YYYY_HH_MI_SS extends AbstractTimestampConverter {
	public static final String PATTERN = "dd-MMM-yyyy hh:mm:ss a";

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
		return ConversionException.DD_MMM_YYYY_HH_MI_SS_KEY;
	}
}
