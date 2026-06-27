package org.skyve.nlp.cron.elementprovider.recurring;

import static org.junit.Assert.assertFalse;

import org.junit.Test;

@SuppressWarnings("static-method")
public class EveryWeekTest {

	@Test
	public void testCanProvideDayOfWeekReturnsFalseWithNoSegments() {
		// no matches() called → segments empty → returns false
		EveryWeek provider = new EveryWeek();
		assertFalse(provider.canProvideDayOfWeek());
	}

}
