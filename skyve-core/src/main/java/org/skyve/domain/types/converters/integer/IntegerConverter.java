package org.skyve.domain.types.converters.integer;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.BeanValidator;

public class IntegerConverter implements Converter<Integer> {
	@Override
	public Integer fromDisplayValue(String displayValue) throws ConversionException {
		try {
            return Integer.valueOf(displayValue);
		}
		catch (NumberFormatException e) {
			if (displayValue.matches("[+]?\\d{11,}")) {
				throw new ConversionException(BeanValidator.VALIDATION_RANGE_GREATER_KEY, displayValue, String.valueOf(Integer.MAX_VALUE));
			}
			else if (displayValue.matches("[-]?\\d{11,}")) {
				throw new ConversionException(BeanValidator.VALIDATION_RANGE_LESS_KEY, displayValue, String.valueOf(Integer.MIN_VALUE));
			}
			throw new ConversionException(ConversionException.INTEGER_CONVERTER_KEY, e);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.INTEGER_CONVERTER_KEY, e);
		}
	}

	@Override
	public String toDisplayValue(Integer value) throws ConversionException {
		try {
			return value.toString();
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.INTEGER_CONVERTER_KEY, e);
		}
	}

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.integer;
	}

	@Override
	public Format<Integer> getFormat() {
		return null;
	}

	@Override
	public Validator<Integer> getValidator() {
		return null;
	}
}
