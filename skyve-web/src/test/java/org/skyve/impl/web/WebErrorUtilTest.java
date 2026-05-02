package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.slf4j.Logger;

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
}
