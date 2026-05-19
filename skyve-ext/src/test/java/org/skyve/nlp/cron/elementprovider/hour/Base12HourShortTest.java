package org.skyve.nlp.cron.elementprovider.hour;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Test;

public class Base12HourShortTest {

	@Test
	@SuppressWarnings("static-method")
	public void testGetHourElementWithoutMatchReturnsNull() {
		// no matches() called → segments empty → size not > 1 → returns null
		Base12HourShort provider = new Base12HourShort();
		assertThat(provider.getHourElement(), is(nullValue()));
	}

}
