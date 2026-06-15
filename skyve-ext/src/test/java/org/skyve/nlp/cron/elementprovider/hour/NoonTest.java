package org.skyve.nlp.cron.elementprovider.hour;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

import java.util.Arrays;
import java.util.Collection;

@SuppressWarnings("static-method")
@RunWith(Parameterized.class)
public class NoonTest {
	@Parameters(name = "{0}")
	public static Collection<Object[]> matchingExpressions() {
		return Arrays.asList(new Object[][] {
				{"noon"},
				{"midday"},
				{"NOON"},
				{"Midday"}
		});
	}

	@Parameter
	public String matchingExpression;

	@Test
	public void testMatchesNoonExpression() {
		Noon provider = new Noon();
		assertTrue(provider.matches(matchingExpression));
	}

	@Test
	public void testNoMatchReturnsFalse() {
		Noon provider = new Noon();
		assertFalse(provider.matches("midnight"));
	}

	@Test
	public void testNullReturnsFalse() {
		Noon provider = new Noon();
		assertFalse(provider.matches(null));
	}

	@Test
	public void testGetHourElementReturnsTwelve() {
		Noon provider = new Noon();
		assertThat(provider.getHourElement(), is("12"));
	}
}
