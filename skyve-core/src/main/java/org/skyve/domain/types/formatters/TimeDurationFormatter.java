package org.skyve.domain.types.formatters;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Decimal5;

/**
 * Format any org.skyve.domain.types.Decimal into a time duration (hours:minutes) representation
 */
public class TimeDurationFormatter implements Formatter<Decimal> {
	private static final MathContext MATH_CONTEXT = new MathContext(4, RoundingMode.HALF_UP);

	@Override
	public Class<Decimal> getValueType() {
		return Decimal.class;
	}
	
	@Override
	public String toDisplayValue(Decimal value) {
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
}
