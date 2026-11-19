package org.skyve.nlp.cron.elementprovider.hour;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

class Base12HourShortTest {

	@Test
	@SuppressWarnings("static-method")
	void testGetHourElementWithoutMatchReturnsNull() {
		// no matches() called → segments empty → size not > 1 → returns null
		Base12HourShort provider = new Base12HourShort();
		assertThat(provider.getHourElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testCanProvideMinuteReturnsFalseWithNoSegments() {
		Base12HourShort provider = new Base12HourShort();
		assertFalse(provider.canProvideMinute());
	}

        @Test
        @SuppressWarnings("static-method")
        void testGetHourElementMidnight12AMReturnsZero() {
                // covers L40: "12am" → segments has 3 entries → "am" branch with hour=12 → returns "0"
                Base12HourShort provider = new Base12HourShort();
                provider.matches("12am");
                assertThat(provider.getHourElement(), is("0"));
        }

}
