package org.skyve.domain.types.converters.decimal.currency;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.ParseException;

import org.skyve.CORE;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class Decimal5DollarsAndCents implements Converter<Decimal5> {
	private static final String PATTERN = "###,###,###,##0.00";

	@Override
	public Class<Decimal5> getValueType() {
		return Decimal5.class;
	}
	
	@Override
	public String toDisplayValue(Decimal5 value) throws ConversionException {
		try {
			DecimalFormat df = CORE.getDecimalFormat(PATTERN);
			df.setParseBigDecimal(true);
			return df.format(value.bigDecimalValue());
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.DECIMAL_5_DOLLARS_AND_CENTS_KEY, e);
		}
	}

	@Override
	public Decimal5 fromDisplayValue(String displayValue) throws ConversionException {
		try {
			String numberValue = displayValue;
			if (displayValue.startsWith("$")) {
				numberValue = displayValue.substring(1).trim();
			}
			DecimalFormat df = CORE.getDecimalFormat(PATTERN);
			df.setParseBigDecimal(true);

			return new Decimal5((BigDecimal) df.parse(numberValue));
		}
		catch (ParseException e) {
			throw new ConversionException(ConversionException.DECIMAL_5_DOLLARS_AND_CENTS_KEY, e);
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
		return PATTERN;
	}
}
