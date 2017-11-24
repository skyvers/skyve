package org.skyve.domain.types;

import java.math.BigDecimal;

public abstract class Decimal extends Number implements Comparable<Number> {
	/**
	 * For Serialization.
	 */
	private static final long serialVersionUID = 2036844906920454892L;

	/**
	 * 
	 */
	protected BigDecimal value;

	/**
	 * 
	 * @param operand
	 * @return
	 */
	public abstract Decimal add(Decimal operand);

	/**
	 * 
	 * @param operand
	 * @return
	 */
	public abstract Decimal divide(Decimal operand);

	/**
	 * 
	 * @param operand
	 * @return
	 */
	public abstract Decimal multiply(Decimal operand);

	/**
	 * 
	 * @param operand
	 * @return
	 */
	public abstract Decimal subtract(Decimal operand);

	/**
	 * 
	 * @return
	 */
	public abstract Decimal abs();

	/**
	 * 
	 * @param power
	 * @return
	 */
	public abstract Decimal pow(int power);

	/**
	 * 
	 * @return
	 */
	public abstract Decimal negate();

	public abstract Decimal min(Decimal other);
	public abstract Decimal max(Decimal other);
	
	/**
	 * 
	 * @return
	 */
	public final int scale() {
		return value.scale();
	}
	
	/**
	 * 
	 * @return
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
	 * 
	 * @return
	 */
	public final int signum() {
		return value.signum();
	}

	/**
	 * 
	 */
	@Override
	public final boolean equals(Object o) {
		if (o instanceof Decimal) {
			return value.equals(((Decimal) o).value);
		}
		else if (o instanceof BigDecimal) {
			return value.equals(o);
		}
		else {
			return false;
		}
	}
	
	/**
	 * 
	 */
	@Override
	public int hashCode() {
		return value.hashCode();
	}

	/**
	 * 
	 */
	@Override
	public int compareTo(Number o) {
		if (o instanceof Decimal) {
			return value.compareTo(((Decimal) o).value);
		}
		else if (o instanceof BigDecimal) {
			return value.compareTo((BigDecimal) o);
		}
		else {
			throw new IllegalArgumentException(o + " needs to be a Decimal or BigDecimal");
		}
	}
	
	public boolean lessThan(Number o) {
		return (compareTo(o) < 0);
	}

	public boolean greaterThan(Number o) {
		return (compareTo(o) > 0);
	}

	/**
	 * 
	 */
	@Override
	public final String toString() {
		return (value == null) ? "null" : value.toString();
	}
}