package org.skyve.nlp.cron.elementprovider.hour;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

class MidnightTest {

	@Test
	@SuppressWarnings("static-method")
	void testGetDayNumberElementReturnsNull() {
		Midnight provider = new Midnight();
		assertThat(provider.getDayNumberElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetMonthElementReturnsNull() {
		Midnight provider = new Midnight();
		assertThat(provider.getMonthElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetDayOfWeekElementReturnsNull() {
		Midnight provider = new Midnight();
		assertThat(provider.getDayOfWeekElement(), is(nullValue()));
	}

}
