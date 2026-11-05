package org.skyve.nlp.cron.elementprovider.hour;

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
class NoonTest {
	static Collection<Object[]> matchingExpressions() {
		return Arrays.asList(new Object[][] {
				{"noon"},
				{"midday"},
				{"NOON"},
				{"Midday"}
		});
	}

	@ParameterizedTest(name = "{0}")
	@MethodSource("matchingExpressions")
	void testMatchesNoonExpression(String matchingExpression) {
		Noon provider = new Noon();
		assertTrue(provider.matches(matchingExpression));
	}

	@Test
	void testNoMatchReturnsFalse() {
		Noon provider = new Noon();
		assertFalse(provider.matches("midnight"));
	}

	@Test
	void testNullReturnsFalse() {
		Noon provider = new Noon();
		assertFalse(provider.matches(null));
	}

	@Test
	void testGetHourElementReturnsTwelve() {
		Noon provider = new Noon();
		assertThat(provider.getHourElement(), is("12"));
	}
}
