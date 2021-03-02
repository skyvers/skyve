package org.skyve.domain.types;

import org.junit.Assert;
import org.junit.Test;

public class Decimal2Test {

	@Test
	@SuppressWarnings("static-method")
	public void testMin() {
		Assert.assertEquals(Decimal2.ONE, Decimal2.ONE.min(Decimal2.ONE_HUNDRED));
		Assert.assertEquals(Decimal2.ONE, Decimal2.ONE_HUNDRED.min(Decimal2.ONE));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testMax() {
		Assert.assertEquals(Decimal2.ONE_HUNDRED, Decimal2.ONE.max(Decimal2.ONE_HUNDRED));
		Assert.assertEquals(Decimal2.ONE_HUNDRED, Decimal2.ONE_HUNDRED.max(Decimal2.ONE));
	}
}
