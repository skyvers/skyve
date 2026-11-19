package org.skyve.nlp.cron.elementprovider;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DayNumberTest {

	@Test
	void testGetDayNumberElementWithNoSegmentsReturnsNull() {
		// no matches() called → segments empty → returns null
		DayNumber provider = new DayNumber();
		assertThat(provider.getDayNumberElement(), is(nullValue()));
	}

	@Test
	void testCanProvideMonthReturnsFalseWithNoSegments() {
		DayNumber provider = new DayNumber();
		assertFalse(provider.canProvideMonth());
	}

}
