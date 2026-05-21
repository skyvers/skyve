package org.skyve.nlp.cron.elementprovider.hour;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertFalse;

import org.junit.Test;

public class Base12HourShortTest {

	@Test
	@SuppressWarnings("static-method")
	public void testGetHourElementWithoutMatchReturnsNull() {
		// no matches() called → segments empty → size not > 1 → returns null
		Base12HourShort provider = new Base12HourShort();
		assertThat(provider.getHourElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testCanProvideMinuteReturnsFalseWithNoSegments() {
		Base12HourShort provider = new Base12HourShort();
		assertFalse(provider.canProvideMinute());
	}

        @Test
        @SuppressWarnings("static-method")
        public void testGetHourElementMidnight12AMReturnsZero() {
                // covers L40: "12am" → segments has 3 entries → "am" branch with hour=12 → returns "0"
                Base12HourShort provider = new Base12HourShort();
                provider.matches("12am");
                assertThat(provider.getHourElement(), is("0"));
        }

}
