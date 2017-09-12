package org.skyve.domain.types;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

/**
 * 
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

	/**
	 * 
	 */
	public static final Decimal2 ZERO = new Decimal2(0);
	
	/**
	 * 
	 */
	public static final Decimal2 ONE = new Decimal2(1);
	
	/**
	 * 
	 */
	public static final Decimal2 TEN = new Decimal2(10);
	
	/**
	 * 
	 */
	public static final Decimal2 ONE_HUNDRED = new Decimal2(100);
	
	/**
	 * 
	 */
	public static final Decimal2 ONE_THOUSAND = new Decimal2(1000);

	/**
	 * 
	 * @param value
	 */
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

	@Override
	public Decimal2 min(Decimal other) {
		return new Decimal2(bigDecimalValue().min(other.bigDecimalValue()));
	}

	@Override
	public Decimal2 max(Decimal other) {
		return new Decimal2(bigDecimalValue().max(other.bigDecimalValue()));
	}
}
