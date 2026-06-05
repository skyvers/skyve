package org.skyve.impl.report;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DefaultReportingTest {
	@Test
	void getReturnsSingletonAndLifecycleMethodsComplete() {
		DefaultReporting reporting = DefaultReporting.get();

		assertSame(reporting, DefaultReporting.get());
		assertDoesNotThrow(reporting::startup);
		assertDoesNotThrow(reporting::shutdown);
	}

	@Test
	void multiReportAttachmentRequiresAtLeastOneReport() {
		DefaultReporting reporting = DefaultReporting.get();

		assertThrows(IllegalArgumentException.class, () -> reporting.getMailAttachmentFromJasperReport(List.of()));
	}
}
