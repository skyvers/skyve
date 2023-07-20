package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class Decimal2Converter implements Converter<Decimal2> {
	@Override
	public Class<Decimal2> getValueType() {
		return Decimal2.class;
	}
	
	@Override
	public Decimal2 fromDisplayValue(String displayValue) throws ConversionException {
		try {
			return new Decimal2(displayValue);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_2_CONVERTER_KEY, e);
		}
	}

	@Override
	public String toDisplayValue(Decimal2 value) throws ConversionException {
		try {
			return value.toString();
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_2_CONVERTER_KEY, e);
		}
	}

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.decimal2;
	}

	@Override
	public Format<Decimal2> getFormat() {
		return null;
	}

	@Override
	public Validator<Decimal2> getValidator() {
		return null;
	}
	
	@Override
	public String getFormatPattern() {
		return null;
	}
}
