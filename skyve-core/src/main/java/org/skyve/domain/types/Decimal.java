package org.skyve.domain.types;

import java.math.BigDecimal;

/**
 * Abstract base class for Skyve's fixed-scale, fixed-precision decimal numbers.
 *
 * <p>Decimal wraps a {@link java.math.BigDecimal} and enforces a fixed scale and
 * precision on every arithmetic result. The concrete subclasses are:
 * <ul>
 *   <li>{@link Decimal2} — 2 decimal places; suitable for currency (e.g. 1.23)
 *   <li>{@link Decimal5} — 5 decimal places; suitable for rates and unit quantities
 *   <li>{@link Decimal10} — 10 decimal places; suitable for scientific or financial precision
 * </ul>
 *
 * <p>All arithmetic operations ({@link #add}, {@link #subtract}, {@link #multiply},
 * {@link #divide}, {@link #abs}, {@link #pow}, {@link #negate}) return a new instance of
 * the same concrete subtype; instances are effectively immutable after construction.
 *
 * <p>Equality ({@link #equals}) is delegated to {@link BigDecimal#equals}, which requires
 * identical scale and value. Use {@link #compareTo} when comparing values across
 * different Decimal subtypes or scales. Use {@link #approximately} for fuzzy equality
 * within a tolerance.
 *
 * <p>Comparison: implements {@link Comparable}{@code <Number>} and accepts both
 * {@link Decimal} and {@link BigDecimal} operands. Passing any other {@link Number}
 * subtype will throw {@link IllegalArgumentException}.
 *
 * <p>Threading: instances are effectively immutable after construction and
 * may be shared across threads.
 */
public abstract class Decimal extends Number implements Comparable<Number> {
	private static final long serialVersionUID = 2036844906920454892L;

	/** The underlying fixed-scale value. Subclasses set the scale and precision on construction. */
	protected BigDecimal value;

	/**
	 * Adds {@code operand} to this value and returns the result as a new instance
	 * of the same concrete subtype with the subtype's fixed scale.
	 *
	 * @param operand the value to add; must not be {@code null}
	 * @return a new instance of the same concrete subtype
	 */
	public abstract Decimal add(Decimal operand);

	/**
	 * Divides this value by {@code operand} and returns the result as a new instance
	 * of the same concrete subtype with the subtype's fixed scale.
	 *
	 * @param operand the divisor; must not be {@code null} and must not be zero
	 * @return a new instance of the same concrete subtype
	 * @throws ArithmeticException if {@code operand} is zero
	 */
	public abstract Decimal divide(Decimal operand);

	/**
	 * Multiplies this value by {@code operand} and returns the result as a new instance
	 * of the same concrete subtype with the subtype's fixed scale.
	 *
	 * @param operand the multiplier; must not be {@code null}
	 * @return a new instance of the same concrete subtype
	 */
	public abstract Decimal multiply(Decimal operand);

	/**
	 * Subtracts {@code operand} from this value and returns the result as a new instance
	 * of the same concrete subtype with the subtype's fixed scale.
	 *
	 * @param operand the value to subtract; must not be {@code null}
	 * @return a new instance of the same concrete subtype
	 */
	public abstract Decimal subtract(Decimal operand);

	/**
	 * Returns the absolute value of this decimal as a new instance of the same
	 * concrete subtype.
	 *
	 * @return a non-negative instance of the same concrete subtype
	 */
	public abstract Decimal abs();

	/**
	 * Raises this value to the given integer power and returns the result as a new
	 * instance of the same concrete subtype.
	 *
	 * @param power the non-negative exponent
	 * @return a new instance of the same concrete subtype
	 * @throws ArithmeticException if {@code power} is negative
	 */
	public abstract Decimal pow(int power);

	/**
	 * Returns the arithmetic negation of this decimal as a new instance of the same
	 * concrete subtype.
	 *
	 * @return a new instance with the sign reversed
	 */
	public abstract Decimal negate();

	/**
	 * Returns the lesser of this value and {@code other}.
	 *
	 * @param other the value to compare with; must not be {@code null}
	 * @return the smaller decimal as a new instance of the same concrete subtype
	 */
	public abstract Decimal min(Decimal other);

	/**
	 * Returns the greater of this value and {@code other}.
	 *
	 * @param other the value to compare with; must not be {@code null}
	 * @return the larger decimal as a new instance of the same concrete subtype
	 */
	public abstract Decimal max(Decimal other);
	
	/**
	 * Returns the scale (number of digits to the right of the decimal point) of
	 * the underlying {@link BigDecimal}.
	 *
	 * @return the scale; always equal to the subtype's fixed scale (2, 5, or 10)
	 */
	public final int scale() {
		return value.scale();
	}
	
	/**
	 * Returns the underlying {@link BigDecimal} value.
	 *
	 * <p>Callers should treat the returned {@code BigDecimal} as read-only; mutating
	 * it via {@code setScale} or similar operations does not affect this instance.
	 *
	 * @return the underlying big decimal; never {@code null}
	 */
	public final BigDecimal bigDecimalValue() {
		return value;
	}

	/**
	 * 
	 */
	@Override
	public final float floatValue() {
		return value.floatValue();
	}

	/**
	 * 
	 */
	@Override
	public final double doubleValue() {
		return value.doubleValue();
	}

	/**
	 * 
	 */
	@Override
	public final int intValue() {
		return value.intValue();
	}

	/**
	 * 
	 */
	@Override
	public final long longValue() {
		return value.longValue();
	}

	/**
	 * Returns the sign of this value: {@code -1}, {@code 0}, or {@code 1}.
	 *
	 * @return negative one, zero, or one as determined by {@link BigDecimal#signum()}
	 */
	public final int signum() {
		return value.signum();
	}

	/**
	 * 
	 */
	@Override
	public final boolean equals(Object o) {
		if (o instanceof Decimal decimal) {
			return value.equals(decimal.value);
		}
		else if (o instanceof BigDecimal) {
			return value.equals(o);
		}
		else {
			return false;
		}
	}
	
	/**
	 * Returns {@code true} if this value and {@code otherValue} differ by no more than
	 * {@code tolerance}.
	 *
	 * <p>Uses {@link #subtract} and {@link #abs} to compute the absolute difference.
	 * Also guards against very large values that cannot be represented as a
	 * {@code double} before comparing with the tolerance.
	 *
	 * @param otherValue the value to compare with; must not be {@code null}
	 * @param tolerance  the maximum permissible absolute difference as a {@code double}
	 * @return {@code true} if {@code |this - otherValue| <= tolerance}
	 */
	public boolean approximately(Decimal otherValue, double tolerance) {
		Decimal difference = this.subtract(otherValue).abs();
		return difference.lessThan(Decimal10.MAX_DOUBLE_VALUE) && (difference.doubleValue() <= tolerance);
	}
	
	/**
	 * 
	 */
	@Override
	public int hashCode() {
		return value.hashCode();
	}

	/**
	 * Compares this value with {@code o}.
	 *
	 * <p>Accepts {@link Decimal} and {@link BigDecimal} operands only.
	 *
	 * @param o the value to compare with; must be a {@link Decimal} or {@link BigDecimal}
	 * @return negative, zero, or positive as defined by {@link BigDecimal#compareTo}
	 * @throws IllegalArgumentException if {@code o} is not a {@link Decimal} or {@link BigDecimal}
	 */
	@Override
	public int compareTo(Number o) {
		if (o instanceof Decimal decimal) {
			return value.compareTo(decimal.value);
		}
		else if (o instanceof BigDecimal bigDecimal) {
			return value.compareTo(bigDecimal);
		}
		else {
			throw new IllegalArgumentException(o + " needs to be a Decimal or BigDecimal");
		}
	}
	
	/**
	 * Returns {@code true} if this value is strictly less than {@code o}.
	 *
	 * @param o the value to compare with; must be a {@link Decimal} or {@link BigDecimal}
	 * @return {@code true} if {@code this < o}
	 */
	public boolean lessThan(Number o) {
		return (compareTo(o) < 0);
	}

	/**
	 * Returns {@code true} if this value is strictly greater than {@code o}.
	 *
	 * @param o the value to compare with; must be a {@link Decimal} or {@link BigDecimal}
	 * @return {@code true} if {@code this > o}
	 */
	public boolean greaterThan(Number o) {
		return (compareTo(o) > 0);
	}

	/**
	 * Returns {@code true} if this value is less than or equal to {@code o}.
	 *
	 * @param o the value to compare with; must be a {@link Decimal} or {@link BigDecimal}
	 * @return {@code true} if {@code this <= o}
	 */
	public boolean lessThanOrEqual(Number o) {
		return (compareTo(o) <= 0);
	}

	/**
	 * Returns {@code true} if this value is greater than or equal to {@code o}.
	 *
	 * @param o the value to compare with; must be a {@link Decimal} or {@link BigDecimal}
	 * @return {@code true} if {@code this >= o}
	 */
	public boolean greaterThanOrEqual(Number o) {
		return (compareTo(o) >= 0);
	}

	/**
	 * Returns the value in plain decimal notation with no engineering or scientific
	 * notation (e.g. {@code 12345.67} rather than {@code 1.234567E+4}).
	 *
	 * @return the plain decimal string; {@code "null"} if the internal value is {@code null}
	 */
	@Override
	public final String toString() {
		return (value == null) ? "null" : value.toPlainString();
	}
}