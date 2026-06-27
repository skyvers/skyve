package org.skyve.domain.types.converters.date;

import org.skyve.domain.messages.ConversionException;

/**
 * Converts {@link org.skyve.domain.types.DateOnly} values using
 * {@code MM/dd/yyyy} formatting.
 */
@SuppressWarnings("java:S101") // Converter class names intentionally match metadata converter IDs.
public class MM_DD_YYYY extends AbstractDateConverter {
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
		return ConversionException.MM_DD_YYYY_KEY;
	}
}
