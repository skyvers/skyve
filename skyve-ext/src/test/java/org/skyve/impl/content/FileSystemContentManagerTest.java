package org.skyve.impl.content;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResults;
import org.skyve.impl.util.UtilImpl;

@SuppressWarnings("static-method")
class FileSystemContentManagerTest {
	private final boolean previousFileStorage = UtilImpl.CONTENT_FILE_STORAGE;

	@AfterEach
	void tearDown() {
		UtilImpl.CONTENT_FILE_STORAGE = previousFileStorage;
	}

	@Test
	void noOpLifecycleAndIndexingMethodsComplete() throws Exception {
		FileSystemContentManager manager = new FileSystemContentManager();

		manager.startup();
		manager.dropIndexing();
		manager.truncateIndexing("customer");
		manager.truncateAttachmentIndexing("customer");
		manager.truncateBeanIndexing("customer");
		manager.reindex(null, true);
		manager.removeBean("biz1");
		manager.close();
		manager.shutdown();
	}

	@Test
	void disabledFileStorageReturnsNullAndIgnoresRemovalAndUpdate() throws Exception {
		UtilImpl.CONTENT_FILE_STORAGE = false;
		FileSystemContentManager manager = new FileSystemContentManager();
		AttachmentContent attachment = new AttachmentContent("demo", "admin", "User", "group1", "user1", "biz1", "avatar");

		assertNull(manager.getAttachment("content1"));
		manager.removeAttachment("content1");
		manager.update(attachment);
	}

	@Test
	void googleReturnsEmptyResultsAndAllReturnsEmptyIterable() throws Exception {
		FileSystemContentManager manager = new FileSystemContentManager();

		SearchResults results = manager.google("anything", 10);
		ContentIterable iterable = manager.all();

		assertFalse(results.getResults().iterator().hasNext());
		assertFalse(iterable.iterator().hasNext());
		assertNull(iterable.iterator().next());
	}
}
