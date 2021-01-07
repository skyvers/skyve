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
	
	@Test
	@SuppressWarnings("static-method")
	public void testApproximately() {
		Assert.assertTrue(Decimal10.ONE.approximately(Decimal10.ONE, 0.0));
		Assert.assertTrue(Decimal10.ONE.approximately(Decimal10.ONE, 0.1));
		Assert.assertFalse(Decimal10.ONE_HUNDRED.approximately(Decimal10.ONE, 0.1));
		Assert.assertFalse(new Decimal10(Double.MAX_VALUE).add(Decimal10.ONE_HUNDRED).approximately(Decimal10.ONE, 1.0));
	}
}
