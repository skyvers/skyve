package org.skyve.domain.types;

import org.junit.Assert;
import org.junit.Test;

public class Decimal5Test {

	@Test
	@SuppressWarnings("static-method")
	public void testMin() {
		Assert.assertEquals(Decimal5.ONE, Decimal5.ONE.min(Decimal5.ONE_HUNDRED));
		Assert.assertEquals(Decimal5.ONE, Decimal5.ONE_HUNDRED.min(Decimal5.ONE));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testMax() {
		Assert.assertEquals(Decimal5.ONE_HUNDRED, Decimal5.ONE.max(Decimal5.ONE_HUNDRED));
		Assert.assertEquals(Decimal5.ONE_HUNDRED, Decimal5.ONE_HUNDRED.max(Decimal5.ONE));
	}
}
