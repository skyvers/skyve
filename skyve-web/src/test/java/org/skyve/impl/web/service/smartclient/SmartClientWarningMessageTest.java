package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class SmartClientWarningMessageTest {
	@Test
	void snapWarningIncludesReferenceAndFriendlyMessage() {
		StringWriter buffer = new StringWriter();
		PrintWriter writer = new PrintWriter(buffer);

		SmartClientSnapServlet.appendUnexpectedWarning("SNAP-123", writer);
		writer.flush();

		String output = buffer.toString();
		assertTrue(output.contains("Snapshot operation was unsuccessful"));
		assertTrue(output.contains("SNAP-123"));
	}

	@Test
	void tagWarningIncludesReferenceAndFriendlyMessage() {
		StringWriter buffer = new StringWriter();
		PrintWriter writer = new PrintWriter(buffer);

		SmartClientTagServlet.appendUnexpectedWarning("TAG-456", writer);
		writer.flush();

		String output = buffer.toString();
		assertTrue(output.contains("tag operation was unsuccessful"));
		assertTrue(output.contains("TAG-456"));
	}
}
