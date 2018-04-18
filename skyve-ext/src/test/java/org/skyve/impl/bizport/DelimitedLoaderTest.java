package org.skyve.impl.bizport;

import org.junit.Assert;
import org.junit.Test;

public class DelimitedLoaderTest {

	@Test
	public void testRemoveQuotesSurroundingString() {
		String input = "\"Jessica, Freindship\"";
		String result = DelimitedLoader.removeQuotesSurroundingString(input);
		Assert.assertEquals("Jessica, Freindship", result);
	}
}
