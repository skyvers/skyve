package org.skyve.domain.types;

import java.math.BigDecimal;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class Decimal5Test {

	@Test
	@SuppressWarnings("static-method")
	void testMin() {
		Assertions.assertEquals(Decimal5.ONE, Decimal5.ONE.min(Decimal5.ONE_HUNDRED));
		Assertions.assertEquals(Decimal5.ONE, Decimal5.ONE_HUNDRED.min(Decimal5.ONE));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMax() {
		Assertions.assertEquals(Decimal5.ONE_HUNDRED, Decimal5.ONE.max(Decimal5.ONE_HUNDRED));
		Assertions.assertEquals(Decimal5.ONE_HUNDRED, Decimal5.ONE_HUNDRED.max(Decimal5.ONE));
	}

	@Test
	@SuppressWarnings("static-method")
	void testPow() {
		Decimal5 result = new Decimal5("2.0").pow(3);
		Assertions.assertEquals(0, new Decimal5("8.0").compareTo(result));
	}

	@Test
	@SuppressWarnings("static-method")
	void testNegate() {
		Decimal5 result = new Decimal5("5.0").negate();
		Assertions.assertEquals(0, new Decimal5("-5.0").compareTo(result));
	}

	@Test
	@SuppressWarnings("static-method")
	void testConstructorFromBigDecimal() {
		Decimal5 result = new Decimal5(new BigDecimal("3.14"));
		Assertions.assertEquals(0, new Decimal5("3.14").compareTo(result));
	}

	@Test
	@SuppressWarnings("static-method")
	void testConstructorFromDecimal() {
		Decimal10 source = new Decimal10("2.5");
		Decimal5 result = new Decimal5(source);
		Assertions.assertEquals(0, new Decimal5("2.5").compareTo(result));
	}
}
