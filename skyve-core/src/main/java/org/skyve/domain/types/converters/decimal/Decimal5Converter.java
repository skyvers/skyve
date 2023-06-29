package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class Decimal5Converter implements Converter<Decimal5> {
	@Override
	public Decimal5 fromDisplayValue(String displayValue) throws ConversionException {
		try {
			return new Decimal5(displayValue);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_5_CONVERTER_KEY, e);
		}
	}

	@Override
	public String toDisplayValue(Decimal5 value) throws ConversionException {
		try {
			return value.toString();
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_5_CONVERTER_KEY, e);
		}
	}

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.decimal5;
	}

	@Override
	public Format<Decimal5> getFormat() {
		return null;
	}

	@Override
	public Validator<Decimal5> getValidator() {
		return null;
	}
	
	@Override
	public String getFormatPattern() {
		return null;
	}
}
