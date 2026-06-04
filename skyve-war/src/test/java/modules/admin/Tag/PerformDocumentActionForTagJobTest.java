package modules.admin.Tag;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

/**
 * Tests simple job branches for tagged document actions.
 */
@SuppressWarnings("static-method")
class PerformDocumentActionForTagJobTest {
	@Test
	void cancelReturnsNull() {
		assertNull(new PerformDocumentActionForTagJob().cancel());
	}
}
