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
 * Converts integer values using grouped thousands separator formatting ({@code 1,234,567}).
 * <p>
 * Parsing reads the whole text, trimmed: an optional {@code +} or {@code -} sign and digits, with optional {@code ,}
 * grouping (read leniently: {@code 1,23,4} is 1234). Anything else, or a value beyond an {@code int}, is refused.
 */
public class IntegerSeparator implements Converter<Integer> {
	/**
	 * Returns the value type handled by this converter.
	 * @return the result value
	 */
	@Override
	public @Nonnull Class<Integer> getValueType() {
		return Integer.class;
	}
	
	/**
	 * Converts a display representation to its domain value.
	 * @param displayValue the display value
	 * @return the result value
	 */
	@Override
	public @Nonnull Integer fromDisplayValue(@Nonnull String displayValue) throws ConversionException {
		try {
			String result = displayValue.trim().replace(",", "");
			return Integer.valueOf(result);
		}
		catch (NumberFormatException e) {
			if (displayValue.matches("[+]?\\d{11,}")) {
				throw new ConversionException(BeanValidator.VALIDATION_RANGE_GREATER_KEY, displayValue, String.valueOf(Integer.MAX_VALUE));
			}
			else if (displayValue.matches("[-]?\\d{11,}")) {
				throw new ConversionException(BeanValidator.VALIDATION_RANGE_LESS_KEY, displayValue, String.valueOf(Integer.MIN_VALUE));
			}
			throw new ConversionException(ConversionException.INTEGER_SEPARATOR_KEY, e);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.INTEGER_SEPARATOR_KEY, e);
		}
	}

	/**
	 * Converts a domain value to its display representation.
	 * @param value the value
	 * @return the result value
	 */
	@Override
	public @Nonnull String toDisplayValue(@Nonnull Integer value) throws ConversionException {
		try {
			return String.format("%,d", value);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.INTEGER_SEPARATOR_KEY, e);
		}
	}

	/**
	 * Returns the attribute type supported by this converter.
	 * @return the result value
	 */
	@Override
	public @Nonnull AttributeType getAttributeType() {
		return AttributeType.integer;
	}

	/**
	 * Returns the optional format descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public @Nullable Format<Integer> getFormat() {
		return null;
	}

	/**
	 * Returns the optional validator descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public @Nullable Validator<Integer> getValidator() {
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
