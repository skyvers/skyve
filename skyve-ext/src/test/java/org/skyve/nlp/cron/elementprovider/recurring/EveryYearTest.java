package org.skyve.nlp.cron.elementprovider.recurring;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Arrays;
import java.util.Collection;

@SuppressWarnings("static-method")
class EveryYearTest {
	static Collection<Object[]> matchingExpressions() {
		return Arrays.asList(new Object[][] {
				{"yearly"},
				{"annually"},
				{"every year"},
				{"each year"},
				{"every 2 years"},
				{"YEARLY"}
		});
	}

	@ParameterizedTest(name = "{0}")
	@MethodSource("matchingExpressions")
	void testMatchesYearExpression(String matchingExpression) {
		EveryYear provider = new EveryYear();
		assertTrue(provider.matches(matchingExpression));
	}

	@Test
	void testNoMatchReturnsFalse() {
		EveryYear provider = new EveryYear();
		assertFalse(provider.matches("daily"));
	}

	@Test
	void testNoMatchEmptyString() {
		EveryYear provider = new EveryYear();
		assertFalse(provider.matches(""));
	}

	@Test
	void testCanProvideMinuteReturnsTrue() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.canProvideMinute());
	}

	@Test
	void testGetMinuteElementReturnsZero() {
		EveryYear provider = new EveryYear();
		assertThat(provider.getMinuteElement(), is("0"));
	}

	@Test
	void testCanProvideHourReturnsTrue() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.canProvideHour());
	}

	@Test
	void testGetHourElementReturnsZero() {
		EveryYear provider = new EveryYear();
		assertThat(provider.getHourElement(), is("0"));
	}

	@Test
	void testCanProvideDayNumberReturnsTrue() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.canProvideDayNumber());
	}

	@Test
	void testGetDayNumberElementReturnsOne() {
		EveryYear provider = new EveryYear();
		assertThat(provider.getDayNumberElement(), is("1"));
	}

	@Test
	void testCanProvideMonthReturnsTrue() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.canProvideMonth());
	}

	@Test
	void testGetMonthElementReturnsOne() {
		EveryYear provider = new EveryYear();
		assertThat(provider.getMonthElement(), is("1"));
	}

	@Test
	void testCanProvideDayOfWeekReturnsTrue() {
		EveryYear provider = new EveryYear();
		assertTrue(provider.canProvideDayOfWeek());
	}

	@Test
	void testGetDayOfWeekElementReturnsStar() {
		EveryYear provider = new EveryYear();
		assertThat(provider.getDayOfWeekElement(), is("*"));
	}

	@Test
	void testIsMinuteElementLockedReturnsFalse() {
		EveryYear provider = new EveryYear();
		assertFalse(provider.isMinuteElementLocked());
	}

	@Test
	void testIsHourElementLockedReturnsFalse() {
		EveryYear provider = new EveryYear();
		assertFalse(provider.isHourElementLocked());
	}

	@Test
	void testIsDayNumberElementLockedReturnsFalse() {
		EveryYear provider = new EveryYear();
		assertFalse(provider.isDayNumberElementLocked());
	}

	@Test
	void testIsMonthElementLockedReturnsFalse() {
		EveryYear provider = new EveryYear();
		assertFalse(provider.isMonthElementLocked());
	}

	@Test
	void testIsDayOfWeekElementLockedReturnsFalse() {
		EveryYear provider = new EveryYear();
		assertFalse(provider.isDayOfWeekElementLocked());
	}
}
