package org.skyve.nlp.cron.elementprovider.recurring;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

class EveryDayNumberTest {

	@Test
	@SuppressWarnings("static-method")
	void testGetMonthElementWithoutMatchReturnsNull() {
		// no matches() called → segments empty → ternary false path returns null (covers L84-86)
		EveryDayNumber provider = new EveryDayNumber();
		assertThat(provider.getMonthElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetDayNumberElementWithoutMatchReturnsNull() {
		EveryDayNumber provider = new EveryDayNumber();
		assertThat(provider.getDayNumberElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanProvideMonthReturnsFalseWithNoSegments() {
		EveryDayNumber provider = new EveryDayNumber();
		assertFalse(provider.canProvideMonth());
	}

}
