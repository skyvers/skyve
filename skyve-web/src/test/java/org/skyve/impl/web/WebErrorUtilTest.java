package org.skyve.impl.web;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
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
		assertEquals("A&B", WebErrorUtil.escapeJsString("A&B"));
		assertEquals("A&amp;B", WebErrorUtil.escapeXmlText("A&B"));
		assertThat(WebErrorUtil.newErrorReference(), instanceOf(String.class));
		assertFalse(WebErrorUtil.newErrorReference().isBlank());
	}
}
