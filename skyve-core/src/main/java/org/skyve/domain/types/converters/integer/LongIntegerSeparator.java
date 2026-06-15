package org.skyve.domain.types.converters.integer;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.BeanValidator;

/**
 * Converts long values using grouped thousands separator formatting.
 */
public class LongIntegerSeparator implements Converter<Long> {
	/**
	 * Returns the value type handled by this converter.
	 * @return the result value
	 */
	@Override
	public Class<Long> getValueType() {
		return Long.class;
	}
	
	/**
	 * Converts a display representation to its domain value.
	 * @param displayValue the display value
	 * @return the result value
	 */
	@Override
	public Long fromDisplayValue(String displayValue) throws ConversionException {
		try {
			String result = displayValue.replace(",", "");
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
	public String toDisplayValue(Long value) throws ConversionException {
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
	public AttributeType getAttributeType() {
		return AttributeType.longInteger;
	}

	/**
	 * Returns the optional format descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public Format<Long> getFormat() {
		return null;
	}

	/**
	 * Returns the optional validator descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public Validator<Long> getValidator() {
		return null;
	}
	
	/**
	 * Returns the optional format pattern for this converter.
	 * @return the result value
	 */
	@Override
	public String getFormatPattern() {
		return null;
	}
}
