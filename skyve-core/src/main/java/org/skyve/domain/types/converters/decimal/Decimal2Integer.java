package org.skyve.domain.types.converters.decimal;

import java.math.BigDecimal;
import java.text.DecimalFormat;

import org.skyve.CORE;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class Decimal2Integer implements Converter<Decimal2> {
	private static final String PATTERN = "###,###,###,##0";

	@Override
	public String toDisplayValue(Decimal2 value) throws ConversionException {
		try {
			DecimalFormat df = CORE.getDecimalFormat(PATTERN);
			df.setParseBigDecimal(true);
			return df.format(value.bigDecimalValue());
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_2_INTEGER_KEY, e);
		}
	}

	@Override
	public Decimal2 fromDisplayValue(String displayValue) throws ConversionException {
		try {
			DecimalFormat df = CORE.getDecimalFormat(PATTERN);
			df.setParseBigDecimal(true);

			return new Decimal2((BigDecimal) df.parse(displayValue));
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_2_INTEGER_KEY, e);
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
		return PATTERN;
	}
}
