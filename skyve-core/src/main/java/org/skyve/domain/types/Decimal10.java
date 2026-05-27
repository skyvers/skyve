package org.skyve.domain.types;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

/**
 * A fixed-scale decimal with 10 decimal places and 28 significant digits of precision.
 *
 * <p>Suitable for scientific values, high-precision financial calculations, and any
 * domain requiring ten decimal places (e.g. {@code 1.2345678901}). All arithmetic
 * results are rounded using {@link java.math.RoundingMode#HALF_UP}.
 *
 * <p>The {@link #MIN_FLOAT_VALUE}, {@link #MAX_FLOAT_VALUE}, {@link #MIN_DOUBLE_VALUE},
 * and {@link #MAX_DOUBLE_VALUE} constants represent the bounds of the corresponding
 * primitive floating-point types expressed as {@code Decimal10} values, useful for
 * range guards and the {@link Decimal#approximately} method.
 *
 * @see Decimal
 * @see Decimal2
 * @see Decimal5
 */
public class Decimal10 extends Decimal {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 4810553835488360301L;

	/**
	 * The precision and rounding mode.
	 */
	private static final MathContext MC = new MathContext(28, RoundingMode.HALF_UP);

	/** Convenience constant for zero with 10 decimal places. */
	public static final Decimal10 ZERO = new Decimal10(0);
	
	/** Convenience constant for one with 10 decimal places. */
	public static final Decimal10 ONE = new Decimal10(1);
	
	/** Convenience constant for one hundred with 10 decimal places. */
	public static final Decimal10 ONE_HUNDRED = new Decimal10(100);

	/** The minimum positive non-zero value representable by a Java {@code float}. */
	public static final Decimal10 MIN_FLOAT_VALUE = new Decimal10(Float.MIN_VALUE);
	/** The maximum finite value representable by a Java {@code float}. */
	public static final Decimal10 MAX_FLOAT_VALUE = new Decimal10(Float.MAX_VALUE);
	/** The minimum positive non-zero value representable by a Java {@code double}. */
	public static final Decimal10 MIN_DOUBLE_VALUE = new Decimal10(Double.MIN_VALUE);
	/** The maximum finite value representable by a Java {@code double}. Used by {@link Decimal#approximately} as an overflow guard. */
	public static final Decimal10 MAX_DOUBLE_VALUE = new Decimal10(Double.MAX_VALUE);
	
	/**
	 * 
	 * @param value
	 */
	@SuppressWarnings("java:S2111") // allow double literal constructor
	public Decimal10(double value) {
		this.value = new BigDecimal(value, MC).setScale(10, MC.getRoundingMode());
	}

	/**
	 * 
	 * @param value
	 */
	public Decimal10(BigDecimal value) {
		this.value = value.setScale(10, MC.getRoundingMode());
	}

	/**
	 * 
	 * @param value
	 */
	public Decimal10(String value) {
		this.value = new BigDecimal(value, MC).setScale(10, MC.getRoundingMode());
	}

	/**
	 * 
	 * @param value
	 */
	public Decimal10(Decimal value) {
		this.value = value.bigDecimalValue().setScale(10, MC.getRoundingMode());
	}

	/**
	 * 
	 */
	@Override
	public Decimal10 add(Decimal operand) {
		return new Decimal10(value.add(operand.bigDecimalValue(), MC));
	}

	/**
	 * 
	 */
	@Override
	public Decimal10 divide(Decimal operand) {
		return new Decimal10(value.divide(operand.bigDecimalValue(), MC));
	}

	/**
	 * 
	 */
	@Override
	public Decimal10 multiply(Decimal operand) {
		return new Decimal10(value.multiply(operand.bigDecimalValue(), MC));
	}

	/**
	 * 
	 */
	@Override
	public Decimal10 subtract(Decimal operand) {
		return new Decimal10(value.subtract(operand.bigDecimalValue(), MC));
	}

	/**
	 * 
	 */
	@Override
	public Decimal10 abs() {
		return new Decimal10(value.abs());
	}

	/**
	 * 
	 */
	@Override
	public Decimal10 pow(int power) {
		return new Decimal10(value.pow(power, MC));
	}

	/**
	 * 
	 */
	@Override
	public Decimal10 negate() {
		return new Decimal10(value.negate());
	}

	/**
	 * Executes min.
	 * @param other the other
	 * @return the result
	 */
	@Override
	public Decimal10 min(Decimal other) {
		return new Decimal10(bigDecimalValue().min(other.bigDecimalValue()));
	}

	/**
	 * Executes max.
	 * @param other the other
	 * @return the result
	 */
	@Override
	public Decimal10 max(Decimal other) {
		return new Decimal10(bigDecimalValue().max(other.bigDecimalValue()));
	}
}
