package modules.whosin;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;

import modules.admin.domain.Contact;
import modules.whosin.domain.WhosinDocument;
import util.AbstractH2Test;

/**
 * Tests that a document the whosin module references from the admin module is addressed through
 * whosin, but resolves to the admin document. The demo customer has the whosin module.
 */
@SuppressWarnings("static-method")
class WhosinDocumentH2Test extends AbstractH2Test {
	@Test
	void aReferencedDocumentIsAddressedThroughTheReferencingModule() {
		Customer demo = ProvidedRepositoryFactory.getNullSafe().getCustomer("demo");
		assertNotNull(demo, "demo");

		Document contact = WhosinDocument.CONTACT.toDocument(demo);

		assertAll(() -> assertEquals("whosin", WhosinDocument.CONTACT.moduleName()),
					() -> assertEquals(Contact.DOCUMENT_NAME, WhosinDocument.CONTACT.documentName()),
					() -> assertEquals(Contact.MODULE_NAME, contact.getOwningModuleName()),
					() -> assertEquals(Contact.DOCUMENT_NAME, contact.getName()));
	}
}
