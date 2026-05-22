package org.skyve.impl.content;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.skyve.content.AttachmentContent;

/**
 * Verifies that {@link NoOpContentManager} can be constructed and that all of
 * its no-op methods complete without throwing.
 */
@SuppressWarnings("resource")
public class NoOpContentManagerTest {

	private NoOpContentManager cm;

	@Before
	public void setUp() {
		cm = new NoOpContentManager();
	}

	@After
	public void tearDown() throws Exception {
		cm.close();
	}

	private static AttachmentContent sampleAttachment() {
		return new AttachmentContent("demo", "admin", "User", null, "user1", "biz1", "photo")
				.attachment("file.pdf", "application/pdf", "bytes".getBytes());
	}

	@Test
	public void testPutBeanContent() throws Exception {
		// BeanContent requires a PersistentBean; pass null since the method is a no-op
		cm.put((org.skyve.content.BeanContent) null);
		assertNotNull(cm);
	}

	@Test
	public void testPutAttachmentContent() throws Exception {
		cm.put(sampleAttachment(), true);
		assertNotNull(cm);
	}

        @Test
        public void testPutAttachmentContentDefaultMethod() throws Exception {
                // Calls the ContentManager default put(AttachmentContent) which delegates to put(attachment, true)
                cm.put(sampleAttachment());
                assertNotNull(cm);
        }

	@Test
	public void testReindexAttachmentContent() throws Exception {
		cm.reindex(sampleAttachment(), false);
		assertNotNull(cm);
	}

	@Test
	public void testGetAttachmentReturnsNull() throws Exception {
		assertNull(cm.getAttachment("cid-123"));
	}

	@Test
	public void testRemoveBean() throws Exception {
		cm.removeBean("biz-1");
		assertNotNull(cm);
	}

	@Test
	public void testRemoveAttachment() throws Exception {
		cm.removeAttachment("cid-1");
		assertNotNull(cm);
	}

	@Test
	public void testGoogleReturnsNull() throws Exception {
		assertNull(cm.google("search term", 10));
	}

	@Test
	public void testDropIndexing() throws Exception {
		cm.dropIndexing();
		assertNotNull(cm);
	}

	@Test
	public void testTruncateIndexing() throws Exception {
		cm.truncateIndexing("demo");
		assertNotNull(cm);
	}

	@Test
	public void testTruncateAttachmentIndexing() throws Exception {
		cm.truncateAttachmentIndexing("demo");
		assertNotNull(cm);
	}

	@Test
	public void testTruncateBeanIndexing() throws Exception {
		cm.truncateBeanIndexing("demo");
		assertNotNull(cm);
	}

	@Test
	public void testAllReturnsNull() throws Exception {
		assertNull(cm.all());
	}

	@Test
	public void testClose() throws Exception {
		cm.close(); // extra close — no-op, must not throw
		assertNotNull(cm);
	}

	@Test
	public void testStartup() {
		cm.startup();
		assertNotNull(cm);
	}

	@Test
	public void testShutdown() {
		cm.shutdown();
		assertNotNull(cm);
	}

        @Test
        public void testUpdateAttachmentContentDoesNotThrow() throws Exception {
                cm.update(sampleAttachment());
                assertNotNull(cm);
        }
}
