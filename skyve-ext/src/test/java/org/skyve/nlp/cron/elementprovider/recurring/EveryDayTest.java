package org.skyve.nlp.cron.elementprovider.recurring;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

class EveryDayTest {

	@Test
	@SuppressWarnings("static-method")
	void testGetDayOfWeekElementWithoutMatchReturnsNull() {
		// no matches() called → segments empty → ternary false path returns null (covers L95)
		EveryDay provider = new EveryDay();
		assertThat(provider.getDayOfWeekElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanProvideDayOfWeekReturnsFalseWithNoSegments() {
		EveryDay provider = new EveryDay();
		assertFalse(provider.canProvideDayOfWeek());
	}

}
