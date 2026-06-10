package org.skyve.impl.content;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResults;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.util.UtilImpl;

@SuppressWarnings("static-method")
class FileSystemContentManagerTest {
	private final boolean previousFileStorage = UtilImpl.CONTENT_FILE_STORAGE;
	private final boolean previousFileSuffixes = UtilImpl.CONTENT_FILE_SUFFIXES;
	private final String previousContentDirectory = UtilImpl.CONTENT_DIRECTORY;

	@TempDir
	private Path tempDir;

	@AfterEach
	void tearDown() {
		UtilImpl.CONTENT_FILE_STORAGE = previousFileStorage;
		UtilImpl.CONTENT_FILE_SUFFIXES = previousFileSuffixes;
		UtilImpl.CONTENT_DIRECTORY = previousContentDirectory;
	}

	@Test
	void noOpLifecycleAndIndexingMethodsComplete() {
		assertDoesNotThrow(() -> {
			try (FileSystemContentManager manager = new FileSystemContentManager()) {
				manager.startup();
				manager.dropIndexing();
				manager.truncateIndexing("customer");
				manager.truncateAttachmentIndexing("customer");
				manager.truncateBeanIndexing("customer");
				manager.reindex(null, true);
				manager.removeBean("biz1");
				manager.shutdown();
			}
		});
	}

	@Test
	void disabledFileStorageReturnsNullAndIgnoresRemovalAndUpdate() throws Exception {
		UtilImpl.CONTENT_FILE_STORAGE = false;
		AttachmentContent attachment = new AttachmentContent("demo", "admin", "User", "group1", "user1", "biz1", "avatar");

		try (FileSystemContentManager manager = new FileSystemContentManager()) {
			assertNull(manager.getAttachment("content1"));
			manager.removeAttachment("content1");
			manager.update(attachment);
		}
	}

	@Test
	void putBeanContentIsNoOp() throws Exception {
		PersistentBean bean = mock(PersistentBean.class);
		when(bean.getBizCustomer()).thenReturn("demo");
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("User");
		when(bean.getBizDataGroupId()).thenReturn("group1");
		when(bean.getBizUserId()).thenReturn("user1");
		when(bean.getBizId()).thenReturn("biz1");

		try (FileSystemContentManager manager = new FileSystemContentManager()) {
			assertDoesNotThrow(() -> manager.put(new BeanContent(bean)));
		}
	}

	@Test
	void putWritesAttachmentContentAndGetAttachmentReadsItBack() throws Exception {
		configureFileStorage();
		AttachmentContent attachment = new AttachmentContent("demo", "admin", "User", "group1", "user1", "biz1", "avatar")
				.attachment("avatar.txt", "text/plain", "hello".getBytes(StandardCharsets.UTF_8))
				.markup("<svg />");

		try (FileSystemContentManager manager = new FileSystemContentManager()) {
			manager.put(attachment, true);

			assertNotNull(attachment.getContentId());
			assertNotNull(attachment.getLastModified());
			AttachmentContent restored = manager.getAttachment(attachment.getContentId());
			assertNotNull(restored);
			assertEquals("avatar.txt", restored.getFileName());
			assertEquals("text/plain", restored.getContentType());
			assertEquals("demo", restored.getBizCustomer());
			assertEquals("admin", restored.getBizModule());
			assertEquals("User", restored.getBizDocument());
			assertEquals("group1", restored.getBizDataGroupId());
			assertEquals("user1", restored.getBizUserId());
			assertEquals("biz1", restored.getBizId());
			assertEquals("avatar", restored.getAttributeName());
			assertEquals("<svg />", restored.getMarkup());
		}
	}

	@Test
	void putStoresExternalFileMetadataAndGetAttachmentUsesExternalFile() throws Exception {
		configureFileStorage();
		File external = tempDir.resolve("external.txt").toFile();
		Files.writeString(external.toPath(), "external content", StandardCharsets.UTF_8);
		AttachmentContent attachment = new AttachmentContent("demo", "admin", "User", null, "user1", "biz1", "file")
				.externalAbsoluteFilePath(external.getAbsolutePath());
		attachment.setContentId("12345678-1234-1234-1234-123456789abc");

		try (FileSystemContentManager manager = new FileSystemContentManager()) {
			manager.put(attachment, false);

			AttachmentContent restored = manager.getAttachment(attachment.getContentId());
			assertNotNull(restored);
			assertEquals("external.txt", restored.getFileName());
			assertEquals("text/plain", restored.getContentType());
			assertEquals("external content", new String(restored.getContentBytes(), StandardCharsets.UTF_8));
		}
	}

	@Test
	void updateRefreshesMetadataForStoredAttachment() throws Exception {
		configureFileStorage();
		AttachmentContent attachment = new AttachmentContent("demo", "admin", "User", "group1", "user1", "biz1", "avatar")
				.attachment("avatar.txt", "text/plain", "hello".getBytes(StandardCharsets.UTF_8));
		attachment.setContentId("12345678-1234-1234-1234-123456789abd");

		try (FileSystemContentManager manager = new FileSystemContentManager()) {
			manager.put(attachment, false);
			attachment.setMarkup("updated");
			manager.update(attachment);

			AttachmentContent restored = manager.getAttachment(attachment.getContentId());
			assertNotNull(restored);
			assertEquals("updated", restored.getMarkup());
		}
	}

	@Test
	void removeAttachmentDeletesStoredAttachment() throws Exception {
		configureFileStorage();
		AttachmentContent attachment = new AttachmentContent("demo", "admin", "User", "group1", "user1", "biz1", "avatar")
				.attachment("avatar.txt", "text/plain", "hello".getBytes(StandardCharsets.UTF_8));
		attachment.setContentId("12345678-1234-1234-1234-123456789abe");

		try (FileSystemContentManager manager = new FileSystemContentManager()) {
			manager.put(attachment, false);
			assertNotNull(manager.getAttachment(attachment.getContentId()));

			manager.removeAttachment(attachment.getContentId());

			assertNull(manager.getAttachment(attachment.getContentId()));
		}
		assertTrue(Files.exists(tempDir));
	}

	@Test
	void googleReturnsEmptyResultsAndAllReturnsEmptyIterable() throws Exception {
		try (FileSystemContentManager manager = new FileSystemContentManager()) {
			SearchResults results = manager.google("anything", 10);
			ContentIterable iterable = manager.all();
			ContentIterable.ContentIterator iterator = iterable.iterator();

			assertFalse(results.getResults().iterator().hasNext());
			assertFalse(iterator.hasNext());
			assertNull(iterator.next());
			assertEquals(0L, iterator.getTotalHits());
		}
	}

	private void configureFileStorage() {
		UtilImpl.CONTENT_FILE_STORAGE = true;
		UtilImpl.CONTENT_FILE_SUFFIXES = true;
		UtilImpl.CONTENT_DIRECTORY = tempDir.toString() + File.separator;
	}
}
