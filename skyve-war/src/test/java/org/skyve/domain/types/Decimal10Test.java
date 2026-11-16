package org.skyve.domain.types;

import java.math.BigDecimal;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class Decimal10Test {

	@Test
	@SuppressWarnings("static-method")
	void testMin() {
		Assertions.assertEquals(Decimal10.ONE, Decimal10.ONE.min(Decimal10.ONE_HUNDRED));
		Assertions.assertEquals(Decimal10.ONE, Decimal10.ONE_HUNDRED.min(Decimal10.ONE));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMax() {
		Assertions.assertEquals(Decimal10.ONE_HUNDRED, Decimal10.ONE.max(Decimal10.ONE_HUNDRED));
		Assertions.assertEquals(Decimal10.ONE_HUNDRED, Decimal10.ONE_HUNDRED.max(Decimal10.ONE));
	}

	@Test
	@SuppressWarnings("static-method")
	void testApproximately() {
		Assertions.assertTrue(Decimal10.ONE.approximately(Decimal10.ONE, 0.0));
		Assertions.assertTrue(Decimal10.ONE.approximately(Decimal10.ONE, 0.1));
		Assertions.assertFalse(Decimal10.ONE_HUNDRED.approximately(Decimal10.ONE, 0.1));
		Assertions.assertFalse(new Decimal10(Double.MAX_VALUE).add(Decimal10.ONE_HUNDRED).approximately(Decimal10.ONE, 1.0));
	}

	@Test
	@SuppressWarnings("static-method")
	void testNoEngineeringNotation() {
		String value = "1234567890.1234567890";
		Assertions.assertEquals(value, new Decimal10(value).toString());
		value = "-1234567890.1234567890";
		Assertions.assertEquals(value, new Decimal10(value).toString());
	}

	@Test
	@SuppressWarnings("static-method")
	void testDivide() {
		Decimal10 result = new Decimal10("10.0").divide(new Decimal10("2.0"));
		Assertions.assertEquals(0, new Decimal10("5.0").compareTo(result));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMultiply() {
		Decimal10 result = new Decimal10("3.0").multiply(new Decimal10("4.0"));
		Assertions.assertEquals(0, new Decimal10("12.0").compareTo(result));
	}

	@Test
	@SuppressWarnings("static-method")
	void testPow() {
		Decimal10 result = new Decimal10("2.0").pow(3);
		Assertions.assertEquals(0, new Decimal10("8.0").compareTo(result));
	}

	@Test
	@SuppressWarnings("static-method")
	void testNegate() {
		Decimal10 result = new Decimal10("5.0").negate();
		Assertions.assertEquals(0, new Decimal10("-5.0").compareTo(result));
	}

	@Test
	@SuppressWarnings("static-method")
	void testConstructorFromBigDecimal() {
		Decimal10 result = new Decimal10(new BigDecimal("3.14"));
		Assertions.assertEquals(0, new Decimal10("3.14").compareTo(result));
	}

	@Test
	@SuppressWarnings("static-method")
	void testConstructorFromDecimal() {
		Decimal5 source = new Decimal5("2.5");
		Decimal10 result = new Decimal10(source);
		Assertions.assertEquals(0, new Decimal10("2.5").compareTo(result));
	}
}
