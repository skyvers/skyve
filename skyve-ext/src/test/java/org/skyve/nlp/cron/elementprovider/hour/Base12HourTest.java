package org.skyve.nlp.cron.elementprovider.hour;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Test;

public class Base12HourTest {

	@Test
	@SuppressWarnings("static-method")
	public void testGetHourElementWithoutMatchReturnsNull() {
		// no matches() called → segments empty → size not > 1 → returns null
		Base12Hour provider = new Base12Hour();
		assertThat(provider.getHourElement(), is(nullValue()));
	}

        @Test
        @SuppressWarnings("static-method")
        public void testGetHourElementMidnight12AMReturnsZero() {
                // covers L30: "12am" with minutes → segments has 4 entries → "am" branch with hour=12 → returns "0"
                Base12Hour provider = new Base12Hour();
                provider.matches("12:00am");
                assertThat(provider.getHourElement(), is("0"));
        }

}
