package org.skyve.domain.types;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

/**
 * A fixed-scale decimal with 2 decimal places and 20 significant digits of precision.
 *
 * <p>Suitable for monetary amounts and any value that requires two decimal places
 * (e.g. {@code 12.34}). All arithmetic results are rounded using
 * {@link java.math.RoundingMode#HALF_UP}.
 *
 * @see Decimal
 * @see Decimal5
 * @see Decimal10
 */
public class Decimal2 extends Decimal {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -7826416961094239278L;

	/**
	 * The precision and rounding mode.
	 */
	private static final MathContext MC = new MathContext(20, RoundingMode.HALF_UP);

	/** Convenience constant for zero with 2 decimal places. */
	public static final Decimal2 ZERO = new Decimal2(0);
	
	/** Convenience constant for one with 2 decimal places. */
	public static final Decimal2 ONE = new Decimal2(1);
	
	/** Convenience constant for ten with 2 decimal places. */
	public static final Decimal2 TEN = new Decimal2(10);
	
	/** Convenience constant for one hundred with 2 decimal places. */
	public static final Decimal2 ONE_HUNDRED = new Decimal2(100);
	
	/** Convenience constant for one thousand with 2 decimal places. */
	public static final Decimal2 ONE_THOUSAND = new Decimal2(1000);

	/**
	 * 
	 * @param value
	 */
	@SuppressWarnings("java:S2111") // allow double literal constructor
	public Decimal2(double value) {
		this.value = new BigDecimal(value, MC).setScale(2, MC.getRoundingMode());
	}

	/**
	 * 
	 * @param value
	 */
	public Decimal2(BigDecimal value) {
		this.value = value.setScale(2, MC.getRoundingMode());
	}

	/**
	 * 
	 * @param value
	 */
	public Decimal2(String value) {
		this.value = new BigDecimal(value, MC).setScale(2, MC.getRoundingMode());
	}

	/**
	 * 
	 * @param value
	 */
	public Decimal2(Decimal value) {
		this.value = value.bigDecimalValue().setScale(2, MC.getRoundingMode());
	}

	/**
	 * 
	 */
	@Override
	public Decimal2 add(Decimal operand) {
		return new Decimal2(value.add(operand.bigDecimalValue(), MC));
	}

	/**
	 * 
	 */
	@Override
	public Decimal2 divide(Decimal operand) {
		return new Decimal2(value.divide(operand.bigDecimalValue(), MC));
	}

	/**
	 * 
	 */
	@Override
	public Decimal2 multiply(Decimal operand) {
		return new Decimal2(value.multiply(operand.bigDecimalValue(), MC));
	}

	/**
	 * 
	 */
	@Override
	public Decimal2 subtract(Decimal operand) {
		return new Decimal2(value.subtract(operand.bigDecimalValue(), MC));
	}

	/**
	 * 
	 */
	@Override
	public Decimal2 abs() {
		return new Decimal2(value.abs());
	}

	/**
	 * 
	 */
	@Override
	public Decimal2 pow(int power) {
		return new Decimal2(value.pow(power, MC));
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Decimal2 negate() {
		return new Decimal2(value.negate());
	}

	/**
	 * Executes min.
	 * @param other the other
	 * @return the result
	 */
	@Override
	public Decimal2 min(Decimal other) {
		return new Decimal2(bigDecimalValue().min(other.bigDecimalValue()));
	}

	/**
	 * Executes max.
	 * @param other the other
	 * @return the result
	 */
	@Override
	public Decimal2 max(Decimal other) {
		return new Decimal2(bigDecimalValue().max(other.bigDecimalValue()));
	}
}
