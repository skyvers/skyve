package org.skyve.domain.types.converters.decimal;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class Decimal5TimeDuration implements Converter<Decimal5> {
	private static final MathContext MATH_CONTEXT = new MathContext(4, RoundingMode.HALF_UP);

	@Override
	public Decimal5 fromDisplayValue(String displayValue) throws Exception {
		int colonIndex = displayValue.indexOf(':');
		if (colonIndex < 0) {
			throw new IllegalArgumentException("Time Duration is not formatted correctly.");
		}

		Decimal5 hours = new Decimal5(displayValue.substring(0, colonIndex));
		Decimal5 minutes = new Decimal5(displayValue.substring(colonIndex + 1));
		minutes = minutes.divide(Decimal5.SIXTY);

		Decimal5 returnValue;
		if (hours.compareTo(Decimal5.ZERO) >= 0) {
			returnValue = hours.add(minutes);
		}
		else {
			returnValue = hours.subtract(minutes);
		}

		return returnValue;
	}

	@Override
	public String toDisplayValue(Decimal5 value) throws Exception {
		BigDecimal decimalValue = value.bigDecimalValue();
		int hours = decimalValue.round(new MathContext(0, RoundingMode.DOWN)).intValue();

		decimalValue = decimalValue.subtract(new BigDecimal(hours));
		decimalValue = decimalValue.multiply(Decimal5.SIXTY.bigDecimalValue(), MATH_CONTEXT);
		decimalValue = decimalValue.round(new MathContext(0, RoundingMode.HALF_UP));
		int minutesValue = (Integer.valueOf(decimalValue.intValue())).intValue();

		// handle round up to 60 minutes
		if (minutesValue == 60) {
			minutesValue = 0;
			hours++;
		}
		if (minutesValue == -60) {
			minutesValue = 0;
			hours--;
		}
		
		// handle negative time
		String sign = "";
		if ((hours < 0) || (minutesValue < 0)) {
			sign = "-";
		}

		String minutes = Integer.valueOf(Math.abs(minutesValue)).toString();
		if (minutes.length() == 1) {
			minutes = '0' + minutes;
		}

		return sign + Math.abs(hours) + ":" + minutes;
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
}
