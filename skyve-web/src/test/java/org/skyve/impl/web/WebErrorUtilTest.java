package org.skyve.impl.web;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.slf4j.Logger;

@SuppressWarnings("static-method")
class WebErrorUtilTest {
	@Test
	void logUnexpectedAndGetReferenceReturnsTheReferenceWrittenToTheServerLog() {
		Logger logger = mock(Logger.class);
		RuntimeException error = new RuntimeException("database table detail");

		String reference = WebErrorUtil.logUnexpectedAndGetReference(logger, "REST request failed", error);

		ArgumentCaptor<String> referenceCaptor = ArgumentCaptor.forClass(String.class);
		verify(logger).error(eq("{} Reference: {}"), eq("REST request failed"), referenceCaptor.capture(), same(error));
		assertEquals(reference, referenceCaptor.getValue());
		assertTrue(WebErrorUtil.genericMessage(reference).endsWith(reference));
	}
	
	@Test
	void logFailureAndGetReferenceLogsAMessageExceptionAsAWarningWithoutAReference() {
		Logger logger = mock(Logger.class);
		ValidationException error = new ValidationException(new Message("Name is required"));

		String reference = WebErrorUtil.logFailureAndGetReference(logger, "SmartClient edit request failed", error);

		assertNull(reference);
		verify(logger).warn("SmartClient edit request failed", error);
		verifyNoMoreInteractions(logger);
	}

	@Test
	void logFailureAndGetReferenceLogsAnythingElseAsAnErrorWithAReference() {
		Logger logger = mock(Logger.class);
		IllegalStateException error = new IllegalStateException("database table detail");

		String reference = WebErrorUtil.logFailureAndGetReference(logger, "SmartClient edit request failed", error);

		assertNotNull(reference);
		verify(logger).error("{} Reference: {}", "SmartClient edit request failed", reference, error);
		verifyNoMoreInteractions(logger);
	}

	@Test
	void appendErrorReferenceUsesTheCorrectSeparatorForQueryUris() {
		String appended = WebErrorUtil.appendErrorReference("/app/view?foo=bar", "abc123");

		assertEquals("/app/view?foo=bar&errorReference=abc123", appended);
	}

	@Test
	void appendErrorReferenceUsesTheCorrectSeparatorForPathUris() {
		String appended = WebErrorUtil.appendErrorReference("/app/view", "abc123");

		assertEquals("/app/view?errorReference=abc123", appended);
	}

	@Test
	void escapeHelpersDelegateToTheUnderlyingEncoders() {
		assertEquals("A&B", WebErrorUtil.escapeJsStringWithHtmlFormatting("A&B"));
		assertEquals("A&amp;B", WebErrorUtil.escapeXmlText("A&B"));
		assertThat(WebErrorUtil.newErrorReference(), instanceOf(String.class));
		assertFalse(WebErrorUtil.newErrorReference().isBlank());
	}
}
