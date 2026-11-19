package org.skyve.nlp.cron.elementprovider.recurring;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class EveryDayNumberStepTest {

	@Test
	void testCanProvideDayNumberReturnsFalseWithNoSegments() {
		// no matches() called → segments empty → size != 3 → returns false
		EveryDayNumberStep provider = new EveryDayNumberStep();
		assertFalse(provider.canProvideDayNumber());
	}

	@Test
	void testGetDayNumberElementReturnsStarWithNoSegments() {
		// no matches() called → segments empty → size <= 2 → returns "*"
		EveryDayNumberStep provider = new EveryDayNumberStep();
		assertThat(provider.getDayNumberElement(), is("*"));
	}

}
