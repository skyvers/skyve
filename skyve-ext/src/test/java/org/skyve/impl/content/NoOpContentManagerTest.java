package org.skyve.impl.content;

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
	}

	@Test
	public void testPutAttachmentContent() throws Exception {
		cm.put(sampleAttachment(), true);
	}

	@Test
	public void testUpdateAttachmentContent() throws Exception {
		cm.update(sampleAttachment());
	}

	@Test
	public void testReindexAttachmentContent() throws Exception {
		cm.reindex(sampleAttachment(), false);
	}

	@Test
	public void testGetAttachmentReturnsNull() throws Exception {
		assertNull(cm.getAttachment("cid-123"));
	}

	@Test
	public void testRemoveBean() throws Exception {
		cm.removeBean("biz-1");
	}

	@Test
	public void testRemoveAttachment() throws Exception {
		cm.removeAttachment("cid-1");
	}

	@Test
	public void testGoogleReturnsNull() throws Exception {
		assertNull(cm.google("search term", 10));
	}

	@Test
	public void testDropIndexing() throws Exception {
		cm.dropIndexing();
	}

	@Test
	public void testTruncateIndexing() throws Exception {
		cm.truncateIndexing("demo");
	}

	@Test
	public void testTruncateAttachmentIndexing() throws Exception {
		cm.truncateAttachmentIndexing("demo");
	}

	@Test
	public void testTruncateBeanIndexing() throws Exception {
		cm.truncateBeanIndexing("demo");
	}

	@Test
	public void testAllReturnsNull() throws Exception {
		assertNull(cm.all());
	}

	@Test
	public void testClose() throws Exception {
		cm.close(); // extra close — no-op, must not throw
	}

	@Test
	public void testStartup() {
		cm.startup();
	}

	@Test
	public void testShutdown() {
		cm.shutdown();
	}
}
