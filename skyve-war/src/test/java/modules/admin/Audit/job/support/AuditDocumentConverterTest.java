package modules.admin.Audit.job.support;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import modules.admin.domain.Audit;

@SuppressWarnings("static-method")
public class AuditDocumentConverterTest {

	@Test
	void handlesWithAuditModuleAndDocumentReturnsTrue() {
		AuditDocumentConverter converter = new AuditDocumentConverter();
		assertTrue(converter.handles(Audit.MODULE_NAME, Audit.DOCUMENT_NAME));
	}

	@Test
	void handlesWithDifferentModuleReturnsFalse() {
		AuditDocumentConverter converter = new AuditDocumentConverter();
		assertFalse(converter.handles("someOtherModule", Audit.DOCUMENT_NAME));
	}

	@Test
	void handlesWithDifferentDocumentReturnsFalse() {
		AuditDocumentConverter converter = new AuditDocumentConverter();
		assertFalse(converter.handles(Audit.MODULE_NAME, "someOtherDocument"));
	}

	@Test
	void handlesWithBothDifferentReturnsFalse() {
		AuditDocumentConverter converter = new AuditDocumentConverter();
		assertFalse(converter.handles("test", "Document"));
	}
}
