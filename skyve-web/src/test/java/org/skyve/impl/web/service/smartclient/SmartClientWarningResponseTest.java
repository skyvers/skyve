package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.junit.jupiter.api.Test;
import org.skyve.impl.web.WebErrorUtil;

class SmartClientWarningResponseTest {
	private static final String REFERENCE = "2f8f0e2c-3b93-4cc2-9d9d-5f24ec777a3d";

	@Test
	void tagUnexpectedWarningContainsGenericReferenceOnly() {
		String warning = appendWarning(SmartClientTagServlet::appendUnexpectedWarning);

		assertTrue(warning.contains("The tag operation was unsuccessful. "));
		assertContainsGenericReferenceOnly(warning);
	}

	@Test
	void snapshotUnexpectedWarningContainsGenericReferenceOnly() {
		String warning = appendWarning(SmartClientSnapServlet::appendUnexpectedWarning);

		assertTrue(warning.contains("The Snapshot operation was unsuccessful. "));
		assertContainsGenericReferenceOnly(warning);
	}

	@Test
	void generatorUnexpectedWarningContainsGenericReferenceOnly() {
		String warning = appendWarning(SmartClientGeneratorServlet::appendUnexpectedWarning);

		assertTrue(warning.contains("Could not generate views. "));
		assertContainsGenericReferenceOnly(warning);
	}

	private static String appendWarning(WarningAppender appender) {
		StringWriter writer = new StringWriter();
		try (PrintWriter printWriter = new PrintWriter(writer)) {
			appender.append(REFERENCE, printWriter);
		}
		return writer.toString();
	}

	private static void assertContainsGenericReferenceOnly(String warning) {
		assertTrue(warning.contains(WebErrorUtil.genericMessage(REFERENCE)));
		assertFalse(warning.contains("QueryException"));
		assertFalse(warning.contains("select *"));
		assertFalse(warning.contains("SECRET_TABLE"));
	}

	@FunctionalInterface
	private interface WarningAppender {
		void append(String reference, PrintWriter printWriter);
	}
}
