package org.skyve.nlp.cron;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

@SuppressWarnings("static-method")
public class CronParserExceptionTest {

	@Test
	public void testConstructorStoresMessage() {
		CronParserException ex = new CronParserException("test message");
		assertThat(ex.getMessage(), is("test message"));
	}

	@Test
	public void testIsRuntimeException() {
		assertTrue(RuntimeException.class.isAssignableFrom(CronParserException.class));
	}

	@Test
	public void testExceptionCanBeThrown() {
		try {
			throw new CronParserException("thrown");
		} catch (CronParserException e) {
			assertNotNull(e);
			assertThat(e.getMessage(), is("thrown"));
		}
	}
}
