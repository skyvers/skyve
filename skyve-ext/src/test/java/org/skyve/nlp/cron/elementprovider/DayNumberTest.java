package org.skyve.nlp.cron.elementprovider;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertFalse;

import org.junit.Test;

@SuppressWarnings("static-method")
public class DayNumberTest {

	@Test
	public void testGetDayNumberElementWithNoSegmentsReturnsNull() {
		// no matches() called → segments empty → returns null
		DayNumber provider = new DayNumber();
		assertThat(provider.getDayNumberElement(), is(nullValue()));
	}

	@Test
	public void testCanProvideMonthReturnsFalseWithNoSegments() {
		DayNumber provider = new DayNumber();
		assertFalse(provider.canProvideMonth());
	}

}
