package org.skyve.domain.types;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

/**
 * 
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

	/**
	 * 
	 */
	public static final Decimal10 ZERO = new Decimal10(0);
	
	/**
	 * 
	 */
	public static final Decimal10 ONE = new Decimal10(1);
	
	/**
	 * 
	 */
	public static final Decimal10 ONE_HUNDRED = new Decimal10(100);

	/**
	 * 
	 * @param value
	 */
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
}
