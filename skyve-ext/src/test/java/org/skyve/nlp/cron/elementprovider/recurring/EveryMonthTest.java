package org.skyve.nlp.cron.elementprovider.recurring;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Test;

public class EveryMonthTest {

	@Test
	@SuppressWarnings("static-method")
	public void testGetMonthElementWithoutMatchReturnsNull() {
		// no matches() called → segments empty → ternary false path returns null (covers L88-90)
		EveryMonth provider = new EveryMonth();
		assertThat(provider.getMonthElement(), is(nullValue()));
	}

}
