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
 * Converts integer values that are a whole number of percent: 12 is shown {@code 12%} (no thousands separator:
 * {@code 1000%}).
 * <p>
 * Parsing reads the whole text, trimmed: an optional {@code +} or {@code -} sign and digits, with or without a
 * {@code %} at the end ({@code 12%} and {@code 12} are 12). Anything else, or a value beyond an {@code int}, is refused.
 */
public class SimplePercentage implements Converter<Integer> {
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
		// a % at the end only: 12% and 12 are 12
		String noPercentage = displayValue.trim();
		if (noPercentage.endsWith("%")) {
			noPercentage = noPercentage.substring(0, noPercentage.length() - 1).trim();
		}
		try {
			return Integer.valueOf(noPercentage);
		}
		catch (NumberFormatException e) {
			if (noPercentage.matches("[+]?\\d{11,}")) {
				throw new ConversionException(BeanValidator.VALIDATION_RANGE_GREATER_KEY, displayValue, String.valueOf(Integer.MAX_VALUE));
			}
			else if (noPercentage.matches("[-]?\\d{11,}")) {
				throw new ConversionException(BeanValidator.VALIDATION_RANGE_LESS_KEY, displayValue, String.valueOf(Integer.MIN_VALUE));
			}
			throw new ConversionException(ConversionException.SIMPLE_PERCENTAGE_KEY, e);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.SIMPLE_PERCENTAGE_KEY, e);
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
			return value + "%";
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.SIMPLE_PERCENTAGE_KEY, e);
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
