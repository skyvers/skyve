package org.skyve.nlp.cron.elementprovider.recurring;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Test;

public class EveryDayTest {

	@Test
	@SuppressWarnings("static-method")
	public void testGetDayOfWeekElementWithoutMatchReturnsNull() {
		// no matches() called → segments empty → ternary false path returns null (covers L95)
		EveryDay provider = new EveryDay();
		assertThat(provider.getDayOfWeekElement(), is(nullValue()));
	}

}
