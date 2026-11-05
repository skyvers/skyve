package org.skyve.nlp.cron;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class CronParserExceptionTest {

	@Test
	void testConstructorStoresMessage() {
		CronParserException ex = new CronParserException("test message");
		assertThat(ex.getMessage(), is("test message"));
	}

	@Test
	void testIsRuntimeException() {
		assertTrue(RuntimeException.class.isAssignableFrom(CronParserException.class));
	}

	@Test
	void testExceptionCanBeThrown() {
		try {
			throw new CronParserException("thrown");
		} catch (CronParserException e) {
			assertNotNull(e);
			assertThat(e.getMessage(), is("thrown"));
		}
	}
}
