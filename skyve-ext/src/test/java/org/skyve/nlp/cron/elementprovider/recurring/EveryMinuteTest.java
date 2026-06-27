package org.skyve.nlp.cron.elementprovider.recurring;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Test;

public class EveryMinuteTest {

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testMatchesEveryMinute() {
		EveryMinute provider = new EveryMinute();
		assertThat(provider.matches("every minute"), is(true));
		assertThat(provider.canProvideMinute(), is(true));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetMinuteElementWithoutMatchReturnsNull() {
		// no matches() called → segments empty → returns null (covers L39)
		EveryMinute provider = new EveryMinute();
		assertThat(provider.getMinuteElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetDayNumberElementReturnsNull() {
		EveryMinute provider = new EveryMinute();
		assertThat(provider.getDayNumberElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetMonthElementReturnsNull() {
		EveryMinute provider = new EveryMinute();
		assertThat(provider.getMonthElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetDayOfWeekElementReturnsNull() {
		EveryMinute provider = new EveryMinute();
		assertThat(provider.getDayOfWeekElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetSecondElementDefaultReturnsNull() {
		// default method on ExpressionElementProvider returns null
		EveryMinute provider = new EveryMinute();
		assertThat(provider.getSecondElement(), is(nullValue()));
	}

}
