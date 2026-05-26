package org.skyve.domain.types.converters.integer;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.BeanValidator;

/**
 * Converts integer values representing simple percentage display values.
 */
public class SimplePercentage implements Converter<Integer> {
	/**
	 * Returns the value type handled by this converter.
	 * @return the result value
	 */
	@Override
	public Class<Integer> getValueType() {
		return Integer.class;
	}
	
	/**
	 * Converts a display representation to its domain value.
	 * @param displayValue the display value
	 * @return the result value
	 */
	@Override
	public Integer fromDisplayValue(String displayValue) throws ConversionException {
		String noPercentage = displayValue.replace("%", "");
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
	public String toDisplayValue(Integer value) throws ConversionException {
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
	public AttributeType getAttributeType() {
		return AttributeType.integer;
	}

	/**
	 * Returns the optional format descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public Format<Integer> getFormat() {
		return null;
	}

	/**
	 * Returns the optional validator descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public Validator<Integer> getValidator() {
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
