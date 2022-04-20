package org.skyve.domain.types.converters.decimal;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class Decimal10Converter implements Converter<Decimal10> {
	@Override
	public Decimal10 fromDisplayValue(String displayValue) throws ConversionException {
		try {
			return new Decimal10(displayValue);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_10_CONVERTER_KEY, e);
		}
	}

	@Override
	public String toDisplayValue(Decimal10 value) throws ConversionException {
		try {
			return value.toString();
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_10_CONVERTER_KEY, e);
		}
	}

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.decimal10;
	}

	@Override
	public Format<Decimal10> getFormat() {
		return null;
	}

	@Override
	public Validator<Decimal10> getValidator() {
		return null;
	}
}
