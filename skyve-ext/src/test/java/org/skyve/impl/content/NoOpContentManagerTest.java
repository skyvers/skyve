package org.skyve.impl.content;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;

/**
 * Verifies that {@link NoOpContentManager} can be constructed and that all of
 * its no-op methods complete without throwing.
 */
@SuppressWarnings("resource")
class NoOpContentManagerTest {

	private NoOpContentManager cm;

	@BeforeEach
	void setUp() {
		cm = new NoOpContentManager();
	}

	@AfterEach
	void tearDown() throws Exception {
		cm.close();
	}

	private static AttachmentContent sampleAttachment() {
		return new AttachmentContent("demo", "admin", "User", null, "user1", "biz1", "photo")
				.attachment("file.pdf", "application/pdf", "bytes".getBytes());
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void testPutBeanContent() throws Exception {
		// BeanContent requires a PersistentBean; pass null since the method is a no-op
		cm.put((org.skyve.content.BeanContent) null);
		assertNotNull(cm);
	}

	@Test
	void testPutAttachmentContent() throws Exception {
		cm.put(sampleAttachment(), true);
		assertNotNull(cm);
	}

        @Test
        void testPutAttachmentContentDefaultMethod() throws Exception {
                // Calls the ContentManager default put(AttachmentContent) which delegates to put(attachment, true)
                cm.put(sampleAttachment());
                assertNotNull(cm);
        }

	@Test
	void testReindexAttachmentContent() throws Exception {
		cm.reindex(sampleAttachment(), false);
		assertNotNull(cm);
	}

	@Test
	void testGetAttachmentReturnsNull() throws Exception {
		assertNull(cm.getAttachment("cid-123"));
	}

	@Test
	void testRemoveBean() throws Exception {
		cm.removeBean("biz-1");
		assertNotNull(cm);
	}

	@Test
	void testRemoveAttachment() throws Exception {
		cm.removeAttachment("cid-1");
		assertNotNull(cm);
	}

	@Test
	void testGoogleReturnsNull() throws Exception {
		assertNull(cm.google("search term", 10));
	}

	@Test
	void testDropIndexing() throws Exception {
		cm.dropIndexing();
		assertNotNull(cm);
	}

	@Test
	void testTruncateIndexing() throws Exception {
		cm.truncateIndexing("demo");
		assertNotNull(cm);
	}

	@Test
	void testTruncateAttachmentIndexing() throws Exception {
		cm.truncateAttachmentIndexing("demo");
		assertNotNull(cm);
	}

	@Test
	void testTruncateBeanIndexing() throws Exception {
		cm.truncateBeanIndexing("demo");
		assertNotNull(cm);
	}

	@Test
	void testAllReturnsNull() throws Exception {
		assertNull(cm.all());
	}

	@Test
	void testClose() throws Exception {
		cm.close(); // extra close — no-op, must not throw
		assertNotNull(cm);
	}

	@Test
	void testStartup() {
		cm.startup();
		assertNotNull(cm);
	}

	@Test
	void testShutdown() {
		cm.shutdown();
		assertNotNull(cm);
	}

        @Test
        void testUpdateAttachmentContentDoesNotThrow() throws Exception {
                cm.update(sampleAttachment());
                assertNotNull(cm);
        }
}
