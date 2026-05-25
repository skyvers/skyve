package org.skyve.nlp.cron.elementprovider.hour;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

@SuppressWarnings("static-method")
public class NoonTest {

	@Test
	public void testMatchesNoon() {
		Noon provider = new Noon();
		assertTrue(provider.matches("noon"));
	}

	@Test
	public void testMatchesMidday() {
		Noon provider = new Noon();
		assertTrue(provider.matches("midday"));
	}

	@Test
	public void testMatchesCaseInsensitiveNoon() {
		Noon provider = new Noon();
		assertTrue(provider.matches("NOON"));
	}

	@Test
	public void testMatchesCaseInsensitiveMidday() {
		Noon provider = new Noon();
		assertTrue(provider.matches("Midday"));
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
