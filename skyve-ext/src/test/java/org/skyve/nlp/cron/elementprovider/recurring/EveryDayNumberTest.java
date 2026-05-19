package org.skyve.nlp.cron.elementprovider.recurring;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Test;

public class EveryDayNumberTest {

	@Test
	@SuppressWarnings("static-method")
	public void testGetMonthElementWithoutMatchReturnsNull() {
		// no matches() called → segments empty → ternary false path returns null (covers L84-86)
		EveryDayNumber provider = new EveryDayNumber();
		assertThat(provider.getMonthElement(), is(nullValue()));
	}

}
