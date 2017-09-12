package org.skyve.domain.types;

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
}
