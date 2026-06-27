package org.skyve.domain.types;

import java.math.BigDecimal;

import org.junit.Assert;
import org.junit.Test;

public class Decimal10Test {

	@Test
	@SuppressWarnings("static-method")
	public void testMin() {
		Assert.assertEquals(Decimal10.ONE, Decimal10.ONE.min(Decimal10.ONE_HUNDRED));
		Assert.assertEquals(Decimal10.ONE, Decimal10.ONE_HUNDRED.min(Decimal10.ONE));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testMax() {
		Assert.assertEquals(Decimal10.ONE_HUNDRED, Decimal10.ONE.max(Decimal10.ONE_HUNDRED));
		Assert.assertEquals(Decimal10.ONE_HUNDRED, Decimal10.ONE_HUNDRED.max(Decimal10.ONE));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testApproximately() {
		Assert.assertTrue(Decimal10.ONE.approximately(Decimal10.ONE, 0.0));
		Assert.assertTrue(Decimal10.ONE.approximately(Decimal10.ONE, 0.1));
		Assert.assertFalse(Decimal10.ONE_HUNDRED.approximately(Decimal10.ONE, 0.1));
		Assert.assertFalse(new Decimal10(Double.MAX_VALUE).add(Decimal10.ONE_HUNDRED).approximately(Decimal10.ONE, 1.0));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testNoEngineeringNotation() {
		String value = "1234567890.1234567890";
		Assert.assertEquals(value, new Decimal10(value).toString());
		value = "-1234567890.1234567890";
		Assert.assertEquals(value, new Decimal10(value).toString());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testDivide() {
		Decimal10 result = new Decimal10("10.0").divide(new Decimal10("2.0"));
		Assert.assertEquals(0, new Decimal10("5.0").compareTo(result));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testMultiply() {
		Decimal10 result = new Decimal10("3.0").multiply(new Decimal10("4.0"));
		Assert.assertEquals(0, new Decimal10("12.0").compareTo(result));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testPow() {
		Decimal10 result = new Decimal10("2.0").pow(3);
		Assert.assertEquals(0, new Decimal10("8.0").compareTo(result));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testNegate() {
		Decimal10 result = new Decimal10("5.0").negate();
		Assert.assertEquals(0, new Decimal10("-5.0").compareTo(result));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testConstructorFromBigDecimal() {
		Decimal10 result = new Decimal10(new BigDecimal("3.14"));
		Assert.assertEquals(0, new Decimal10("3.14").compareTo(result));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testConstructorFromDecimal() {
		Decimal5 source = new Decimal5("2.5");
		Decimal10 result = new Decimal10(source);
		Assert.assertEquals(0, new Decimal10("2.5").compareTo(result));
	}
}
