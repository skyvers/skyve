package org.skyve.domain.types.formatters;

import java.math.BigDecimal;
import java.math.RoundingMode;

import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Decimal5;

import jakarta.annotation.Nonnull;

/**
 * Format any org.skyve.domain.types.Decimal into a time duration (hours:minutes) representation: the hours truncated
 * towards zero, then the minutes rounded once, half up (away from zero), from the exact fraction of an hour, with a sign
 * for a negative duration: 1.26 is 1:16, 0.49166 is 0:29, 0.99999 is 1:00 and -0.01 is -0:01.
 */
public class TimeDurationFormatter implements Formatter<Decimal> {

	/**
	 * Returns the valueType.
	 * @return the result
	 */
	@Override
	public @Nonnull Class<Decimal> getValueType() {
		return Decimal.class;
	}
	
	/**
	 * Executes toDisplayValue.
	 * @param value the value
	 * @return the result
	 */
	@Override
	public @Nonnull String toDisplayValue(@Nonnull Decimal value) {
		BigDecimal decimalValue = value.bigDecimalValue();
		// truncated towards zero
		int hours = decimalValue.intValue();

		// the exact minutes (a decimal times 60 needs no rounding), rounded once to the nearest minute, half up
		decimalValue = decimalValue.subtract(new BigDecimal(hours));
		decimalValue = decimalValue.multiply(Decimal5.SIXTY.bigDecimalValue());
		int minutesValue = decimalValue.setScale(0, RoundingMode.HALF_UP).intValue();

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
