package org.skyve.domain.types.converters.integer;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.BeanValidator;

public class LongIntegerConverter implements Converter<Long> {
	@Override
	public Class<Long> getValueType() {
		return Long.class;
	}
	
	@Override
	public Long fromDisplayValue(String displayValue) throws ConversionException {
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

	@Override
	public String toDisplayValue(Long value) throws ConversionException {
		try {
			return value.toString();
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.LONG_INTEGER_CONVERTER_KEY, e);
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
	
	@Override
	public String getFormatPattern() {
		return null;
	}
}
