package org.skyve.nlp.cron.elementprovider.recurring;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

@SuppressWarnings("static-method")
public class EveryYearTest {

	@Test
	public void testMatchesYearly() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.matches("yearly"));
	}

	@Test
	public void testMatchesAnnually() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.matches("annually"));
	}

	@Test
	public void testMatchesEveryYear() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.matches("every year"));
	}

	@Test
	public void testMatchesEachYear() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.matches("each year"));
	}

	@Test
	public void testMatchesEvery2Years() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.matches("every 2 years"));
	}

	@Test
	public void testMatchesCaseInsensitive() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.matches("YEARLY"));
	}

	@Test
	public void testNoMatchReturnsFalse() {
		EveryYear provider = new EveryYear();
		assertFalse(provider.matches("daily"));
	}

	@Test
	public void testNoMatchEmptyString() {
		EveryYear provider = new EveryYear();
		assertFalse(provider.matches(""));
	}

	@Test
	public void testCanProvideMinuteReturnsTrue() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.canProvideMinute());
	}

	@Test
	public void testGetMinuteElementReturnsZero() {
		EveryYear provider = new EveryYear();
		assertThat(provider.getMinuteElement(), is("0"));
	}

	@Test
	public void testCanProvideHourReturnsTrue() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.canProvideHour());
	}

	@Test
	public void testGetHourElementReturnsZero() {
		EveryYear provider = new EveryYear();
		assertThat(provider.getHourElement(), is("0"));
	}

	@Test
	public void testCanProvideDayNumberReturnsTrue() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.canProvideDayNumber());
	}

	@Test
	public void testGetDayNumberElementReturnsOne() {
		EveryYear provider = new EveryYear();
		assertThat(provider.getDayNumberElement(), is("1"));
	}

	@Test
	public void testCanProvideMonthReturnsTrue() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.canProvideMonth());
	}

	@Test
	public void testGetMonthElementReturnsOne() {
		EveryYear provider = new EveryYear();
		assertThat(provider.getMonthElement(), is("1"));
	}

	@Test
	public void testCanProvideDayOfWeekReturnsTrue() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.canProvideDayOfWeek());
	}

	@Test
	public void testGetDayOfWeekElementReturnsStar() {
		EveryYear provider = new EveryYear();
		assertThat(provider.getDayOfWeekElement(), is("*"));
	}

	@Test
	public void testIsMinuteElementLockedReturnsFalse() {
		EveryYear provider = new EveryYear();
		assertFalse(provider.isMinuteElementLocked());
	}

	@Test
	public void testIsHourElementLockedReturnsFalse() {
		EveryYear provider = new EveryYear();
		assertFalse(provider.isHourElementLocked());
	}

	@Test
	public void testIsDayNumberElementLockedReturnsFalse() {
		EveryYear provider = new EveryYear();
		assertFalse(provider.isDayNumberElementLocked());
	}

	@Test
	public void testIsMonthElementLockedReturnsFalse() {
		EveryYear provider = new EveryYear();
		assertFalse(provider.isMonthElementLocked());
	}

	@Test
	public void testIsDayOfWeekElementLockedReturnsFalse() {
		EveryYear provider = new EveryYear();
		assertFalse(provider.isDayOfWeekElementLocked());
	}
}
