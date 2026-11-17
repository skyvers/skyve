package org.skyve.domain.types.converters.integer;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.BeanValidator;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Converts long values using grouped thousands separator formatting ({@code 12,345,678,901}).
 * <p>
 * Parsing reads the whole text, trimmed: an optional {@code +} or {@code -} sign and digits, with optional {@code ,}
 * grouping (read leniently). Anything else, or a value beyond a {@code long}, is refused.
 */
public class LongIntegerSeparator implements Converter<Long> {
	/**
	 * Returns the value type handled by this converter.
	 * @return the result value
	 */
	@Override
	public @Nonnull Class<Long> getValueType() {
		return Long.class;
	}
	
	/**
	 * Converts a display representation to its domain value.
	 * @param displayValue the display value
	 * @return the result value
	 */
	@Override
	public @Nonnull Long fromDisplayValue(@Nonnull String displayValue) throws ConversionException {
		try {
			String result = displayValue.trim().replace(",", "");
			return Long.valueOf(result);
		}
		catch (NumberFormatException e) {
			if (displayValue.matches("[+]?\\d{11,}")) {
				throw new ConversionException(BeanValidator.VALIDATION_RANGE_GREATER_KEY, displayValue, String.valueOf(Long.MAX_VALUE));
			}
			else if (displayValue.matches("[-]?\\d{11,}")) {
				throw new ConversionException(BeanValidator.VALIDATION_RANGE_LESS_KEY, displayValue, String.valueOf(Long.MIN_VALUE));
			}
			throw new ConversionException(ConversionException.LONG_INTEGER_SEPARATOR_KEY, e);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.LONG_INTEGER_SEPARATOR_KEY, e);
		}
	}

	/**
	 * Converts a domain value to its display representation.
	 * @param value the value
	 * @return the result value
	 */
	@Override
	public @Nonnull String toDisplayValue(@Nonnull Long value) throws ConversionException {
		try {
			return String.format("%,d",value);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.LONG_INTEGER_SEPARATOR_KEY, e);
		}
	}

	/**
	 * Returns the attribute type supported by this converter.
	 * @return the result value
	 */
	@Override
	public @Nonnull AttributeType getAttributeType() {
		return AttributeType.longInteger;
	}

	/**
	 * Returns the optional format descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public @Nullable Format<Long> getFormat() {
		return null;
	}

	/**
	 * Returns the optional validator descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public @Nullable Validator<Long> getValidator() {
		return null;
	}
	
	/**
	 * Returns the optional format pattern for this converter.
	 * @return the result value
	 */
	@Override
	public @Nullable String getFormatPattern() {
		return null;
	}
}
