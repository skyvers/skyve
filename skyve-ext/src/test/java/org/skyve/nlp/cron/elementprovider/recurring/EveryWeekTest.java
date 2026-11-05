package org.skyve.nlp.cron.elementprovider.recurring;

import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class EveryWeekTest {

	@Test
	void testCanProvideDayOfWeekReturnsFalseWithNoSegments() {
		// no matches() called → segments empty → returns false
		EveryWeek provider = new EveryWeek();
		assertFalse(provider.canProvideDayOfWeek());
	}

}
