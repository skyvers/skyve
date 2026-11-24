package org.skyve.report;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.ModuleDocument;
import org.skyve.util.MailAttachment;

/**
 * Tests that the typed-reference default methods of {@link Reporting} delegate to the String
 * forms with the reference's module name and document name.
 */
@SuppressWarnings("static-method")
class ReportingTest {
	/** Lets Mockito's subclass mock maker call the interface's default methods for real. */
	private abstract static class ReportingDouble implements Reporting {
		// no state
	}

	@Test
	void typedGetMailAttachmentFromJasperReportDelegatesToTheStringForm() throws Exception {
		Reporting reporting = mock(ReportingDouble.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		Map<String, Object> parameters = Map.of("p", "v");
		MailAttachment attachment = new MailAttachment();
		doReturn(attachment).when(reporting).getMailAttachmentFromJasperReport("sales", "Order", "rOrders", parameters);

		assertSame(attachment, reporting.getMailAttachmentFromJasperReport(SalesDocument.ORDER, "rOrders", parameters));
	}

	private enum SalesDocument implements ModuleDocument {
		ORDER;

		@Override
		public String moduleName() {
			return "sales";
		}

		@Override
		public String documentName() {
			return "Order";
		}
	}
}
