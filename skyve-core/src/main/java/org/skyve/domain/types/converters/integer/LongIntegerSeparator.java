package org.skyve.domain.types.converters.integer;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.BeanValidator;

public class LongIntegerSeparator implements Converter<Long> {
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

	@Override
	public String toDisplayValue(Long value) throws ConversionException {
		try {
			return String.format("%,d",value);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.LONG_INTEGER_SEPARATOR_KEY, e);
		}
	}

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.longInteger;
	}

	@Override
	public Format<Long> getFormat() {
		return null;
	}

	@Override
	public Validator<Long> getValidator() {
		return null;
	}
}
