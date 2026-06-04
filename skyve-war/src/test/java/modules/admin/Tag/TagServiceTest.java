package modules.admin.Tag;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import modules.admin.domain.User;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class TagServiceTest extends AbstractH2Test {
	@Test
	void setOperationsIgnoreNullOperands() {
		TagService service = new TagService();
		TagExtension tag = new TagExtension();

		assertDoesNotThrow(() -> service.union(null, tag));
		assertDoesNotThrow(() -> service.union(tag, null));
		assertDoesNotThrow(() -> service.intersect(null, tag));
		assertDoesNotThrow(() -> service.intersect(tag, null));
		assertDoesNotThrow(() -> service.except(null, tag));
		assertDoesNotThrow(() -> service.except(tag, null));
	}

	@Test
	void getTaggedItemsForDocumentReturnsEmptyWhenInputsAreIncomplete() throws Exception {
		TagService service = new TagService();

		assertTrue(service.getTaggedItemsForDocument(new TagExtension(), null, User.DOCUMENT_NAME).isEmpty());
		assertTrue(service.getTaggedItemsForDocument(new TagExtension(), User.MODULE_NAME, null).isEmpty());
		assertTrue(service.getTaggedItemsForDocument(null, User.MODULE_NAME, User.DOCUMENT_NAME).isEmpty());
	}
}
