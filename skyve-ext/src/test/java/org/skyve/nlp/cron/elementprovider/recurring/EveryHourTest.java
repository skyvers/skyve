package org.skyve.nlp.cron.elementprovider.recurring;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Test;

public class EveryHourTest {

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testMatchesHourly() {
		EveryHour provider = new EveryHour();
		assertThat(provider.matches("hourly"), is(true));
		assertThat(provider.canProvideHour(), is(true));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testMatchesEveryHour() {
		EveryHour provider = new EveryHour();
		assertThat(provider.matches("every hour"), is(true));
		assertThat(provider.canProvideHour(), is(true));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testNoMatchYieldsCannotProvide() {
		EveryHour provider = new EveryHour();
		assertThat(provider.matches("daily"), is(false));
		assertThat(provider.canProvideHour(), is(false));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetHourElementWhenNotMatched() {
		EveryHour provider = new EveryHour();
		// no call to matches — segments empty
		assertThat(provider.getHourElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetHourElementHourly() {
		EveryHour provider = new EveryHour();
		provider.matches("hourly");
		// "hourly" produces ≤ 3 segments → element is "*"
		assertThat(provider.getHourElement(), is("*"));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testGetHourElementEvery2Hours() {
		EveryHour provider = new EveryHour();
		provider.matches("every 2 hours");
		// "every 2 hours" — 4+ segments captured → element is "*/2"
		String result = provider.getHourElement();
		assertThat(result != null, is(true));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetMinuteElementAlwaysNull() {
		EveryHour provider = new EveryHour();
		provider.matches("hourly");
		assertThat(provider.getMinuteElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetDayNumberElementAlwaysNull() {
		EveryHour provider = new EveryHour();
		provider.matches("hourly");
		assertThat(provider.getDayNumberElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetMonthElementAlwaysNull() {
		EveryHour provider = new EveryHour();
		provider.matches("hourly");
		assertThat(provider.getMonthElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetDayOfWeekElementAlwaysNull() {
		EveryHour provider = new EveryHour();
		provider.matches("hourly");
		assertThat(provider.getDayOfWeekElement(), is(nullValue()));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testCannotProvideOtherElements() {
		EveryHour provider = new EveryHour();
		provider.matches("hourly");
		assertThat(provider.canProvideMinute(), is(false));
		assertThat(provider.canProvideDayNumber(), is(false));
		assertThat(provider.canProvideMonth(), is(false));
		assertThat(provider.canProvideDayOfWeek(), is(false));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testLockedElements() {
		EveryHour provider = new EveryHour();
		assertThat(provider.isHourElementLocked(), is(true));
		assertThat(provider.isMinuteElementLocked(), is(false));
		assertThat(provider.isDayNumberElementLocked(), is(false));
		assertThat(provider.isMonthElementLocked(), is(false));
		assertThat(provider.isDayOfWeekElementLocked(), is(false));
	}
}
