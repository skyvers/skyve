package org.skyve.domain.types;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

/**
 * A fixed-scale decimal with 5 decimal places and 23 significant digits of precision.
 *
 * <p>Suitable for rates, quantities, and other values requiring five decimal places
 * (e.g. {@code 1.23456}). All arithmetic results are rounded using
 * {@link java.math.RoundingMode#HALF_UP}.
 *
 * @see Decimal
 * @see Decimal2
 * @see Decimal10
 */
public class Decimal5 extends Decimal {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -3271778157707069682L;

	/**
	 * The precision and rounding mode.
	 */
	private static final MathContext MC = new MathContext(23, RoundingMode.HALF_UP);

	/** Convenience constant for zero with 5 decimal places. */
	public static final Decimal5 ZERO = new Decimal5(0);
	
	/** Convenience constant for one with 5 decimal places. */
	public static final Decimal5 ONE = new Decimal5(1);
	
	/** Convenience constant for sixty with 5 decimal places; useful for time-unit conversions. */
	public static final Decimal5 SIXTY = new Decimal5(60);
	
	/** Convenience constant for one hundred with 5 decimal places. */
	public static final Decimal5 ONE_HUNDRED = new Decimal5(100);

	/**
	 * 
	 * @param value
	 */
	@SuppressWarnings("java:S2111") // allow double literal constructor
	public Decimal5(double value) {
		this.value = new BigDecimal(value, MC).setScale(5, MC.getRoundingMode());
	}

	/**
	 * 
	 * @param value
	 */
	public Decimal5(BigDecimal value) {
		this.value = value.setScale(5, MC.getRoundingMode());
	}

	/**
	 * 
	 * @param value
	 */
	public Decimal5(String value) {
		this.value = new BigDecimal(value, MC).setScale(5, MC.getRoundingMode());
	}

	/**
	 * 
	 * @param value
	 */
	public Decimal5(Decimal value) {
		this.value = value.bigDecimalValue().setScale(5, MC.getRoundingMode());
	}

	/**
	 * 
	 */
	@Override
	public Decimal5 add(Decimal operand) {
		return new Decimal5(value.add(operand.bigDecimalValue(), MC));
	}

	/**
	 * 
	 */
	@Override
	public Decimal5 divide(Decimal operand) {
		return new Decimal5(value.divide(operand.bigDecimalValue(), MC));
	}

	/**
	 * 
	 */
	@Override
	public Decimal5 multiply(Decimal operand) {
		return new Decimal5(value.multiply(operand.bigDecimalValue(), MC));
	}

	/**
	 * 
	 */
	@Override
	public Decimal5 subtract(Decimal operand) {
		return new Decimal5(value.subtract(operand.bigDecimalValue(), MC));
	}

	/**
	 * 
	 */
	@Override
	public Decimal5 abs() {
		return new Decimal5(value.abs());
	}

	/**
	 * 
	 */
	@Override
	public Decimal5 pow(int power) {
		return new Decimal5(value.pow(power, MC));
	}

	/**
	 * 
	 */
	@Override
	public Decimal5 negate() {
		return new Decimal5(value.negate());
	}
	
	/**
	 * Executes min.
	 * @param other the other
	 * @return the result
	 */
	@Override
	public Decimal5 min(Decimal other) {
		return new Decimal5(bigDecimalValue().min(other.bigDecimalValue()));
	}

	/**
	 * Executes max.
	 * @param other the other
	 * @return the result
	 */
	@Override
	public Decimal5 max(Decimal other) {
		return new Decimal5(bigDecimalValue().max(other.bigDecimalValue()));
	}
}
