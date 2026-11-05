package org.skyve.domain.types;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class Decimal2Test {

	@Test
	@SuppressWarnings("static-method")
	void testMin() {
		Assertions.assertEquals(Decimal2.ONE, Decimal2.ONE.min(Decimal2.ONE_HUNDRED));
		Assertions.assertEquals(Decimal2.ONE, Decimal2.ONE_HUNDRED.min(Decimal2.ONE));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMax() {
		Assertions.assertEquals(Decimal2.ONE_HUNDRED, Decimal2.ONE.max(Decimal2.ONE_HUNDRED));
		Assertions.assertEquals(Decimal2.ONE_HUNDRED, Decimal2.ONE_HUNDRED.max(Decimal2.ONE));
	}
}
