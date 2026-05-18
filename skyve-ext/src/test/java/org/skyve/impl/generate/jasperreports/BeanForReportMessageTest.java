package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

class BeanForReportMessageTest {

	@Test
	void getMessageWithLiteralStringReturnsUnchanged() {
		// Binder.formatMessage with no binding placeholders returns the message unchanged.
		// We use a null bean because there are no placeholders to resolve.
		String message = "Hello, world!";
		String result = BeanForReport.getMessage(null, message);
		assertThat(result, notNullValue());
		assertThat(result, is(message));
	}

	@Test
	void getMessageWithNullMessageThrowsNPE() {
		// BindUtil.formatMessage(null, bean) does not guard against a null message string
		org.junit.jupiter.api.Assertions.assertThrows(
				NullPointerException.class,
				() -> BeanForReport.getMessage(null, null));
	}
}
