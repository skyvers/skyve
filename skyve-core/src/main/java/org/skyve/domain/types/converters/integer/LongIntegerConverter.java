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
 * Converts long integer values using plain long formatting/parsing.
 */
public class LongIntegerConverter implements Converter<Long> {
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
            return Long.valueOf(displayValue);
		}
		catch (NumberFormatException e) {
			if (displayValue.matches("[+]?\\d{19,}")) {
				throw new ConversionException(BeanValidator.VALIDATION_RANGE_GREATER_KEY, displayValue, String.valueOf(Long.MAX_VALUE));
			}
			else if (displayValue.matches("[-]?\\d{19,}")) {
				throw new ConversionException(BeanValidator.VALIDATION_RANGE_LESS_KEY, displayValue, String.valueOf(Long.MIN_VALUE));
			}
			throw new ConversionException(ConversionException.LONG_INTEGER_CONVERTER_KEY, e);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.LONG_INTEGER_CONVERTER_KEY, e);
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
			return value.toString();
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.LONG_INTEGER_CONVERTER_KEY, e);
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
