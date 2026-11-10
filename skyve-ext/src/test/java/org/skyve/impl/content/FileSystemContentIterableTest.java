package org.skyve.impl.content;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentIterable.ContentIterator;
import org.skyve.content.ContentManager;
import org.skyve.content.SearchResult;
import org.skyve.impl.util.UtilImpl;

/**
 * Verifies the walk of the file system content store behind {@link FileSystemContentManager#all()}.
 */
@SuppressWarnings("static-method")
class FileSystemContentIterableTest {
	private final boolean previousFileStorage = UtilImpl.CONTENT_FILE_STORAGE;
	private final String previousContentDirectory = UtilImpl.CONTENT_DIRECTORY;

	@TempDir
	private Path tempDir;

	@BeforeEach
	void setUp() {
		UtilImpl.CONTENT_FILE_STORAGE = true;
		UtilImpl.CONTENT_DIRECTORY = tempDir.toString() + File.separator;
	}

	@AfterEach
	void tearDown() {
		UtilImpl.CONTENT_FILE_STORAGE = previousFileStorage;
		UtilImpl.CONTENT_DIRECTORY = previousContentDirectory;
	}

	@Test
	void returnsEachStoredAttachmentWithItsMetaFields() throws Exception {
		Date lastModified = new Date(1_700_000_000_000L);
		String contentId = put("biz1", lastModified);
		String otherContentId = put("biz2", lastModified);

		List<SearchResult> hits = all();

		assertEquals(2, hits.size());
		assertTrue(hits.stream().anyMatch(hit -> otherContentId.equals(hit.getContentId())));
		SearchResult hit = hits.stream().filter(h -> contentId.equals(h.getContentId())).findFirst().orElseThrow();
		assertTrue(hit.isAttachment());
		assertEquals("demo", hit.getCustomerName());
		assertEquals("admin", hit.getModuleName());
		assertEquals("User", hit.getDocumentName());
		assertEquals("group1", hit.getBizDataGroupId());
		assertEquals("user1", hit.getBizUserId());
		assertEquals("biz1", hit.getBizId());
		assertEquals("avatar", hit.getAttributeName());
		assertEquals(lastModified, hit.getLastModified());
	}

	@Test
	void walksTheStoreInFolderPathOrder() throws Exception {
		// folders 90/00, 90/b0 and a0: not the content ID order
		String first = "99999000-0000-0000-0000-000000000000";
		String second = "99999000-0b00-0000-0000-000000000000";
		String third = "00000a00-0000-0000-0000-000000000000";
		for (String contentId : List.of(second, third, first)) {
			put("biz", new Date(), contentId);
		}

		assertEquals(List.of(first, second, third), all().stream().map(SearchResult::getContentId).toList());
	}

	@Test
	void returnsWhatTheContentManagerReadsBack() throws Exception {
		put("biz1", new Date());

		SearchResult hit = all().get(0);
		try (FileSystemContentManager manager = new FileSystemContentManager()) {
			AttachmentContent attachment = manager.getAttachment(hit.getContentId());

			assertNotNull(attachment);
			assertEquals(attachment.getBizId(), hit.getBizId());
			assertEquals(attachment.getAttributeName(), hit.getAttributeName());
			assertEquals(attachment.getLastModified(), hit.getLastModified());
		}
	}

	@Test
	void returnsExternalContentFromItsMetaFile() throws Exception {
		File external = tempDir.resolve("external.txt").toFile();
		Files.writeString(external.toPath(), "external content", StandardCharsets.UTF_8);
		AttachmentContent attachment = new AttachmentContent("demo", "admin", "User", null, "user1", "biz1", "file")
				.externalAbsoluteFilePath(external.getAbsolutePath());
		try (FileSystemContentManager manager = new FileSystemContentManager()) {
			manager.put(attachment, false);
		}

		List<SearchResult> hits = all();

		assertEquals(1, hits.size());
		assertEquals(attachment.getContentId(), hits.get(0).getContentId());
		assertEquals("file", hits.get(0).getAttributeName());
	}

	@Test
	void skipsFoldersAndFilesThatAreNotReadableContent() throws Exception {
		String contentId = put("biz1", new Date());
		File store = store();

		// a content folder with no meta.json
		Files.createDirectories(new File(store, contentFolder(UUID.randomUUID().toString())).toPath());
		// a content folder with a malformed meta.json
		File malformed = new File(store, contentFolder(UUID.randomUUID().toString()));
		Files.createDirectories(malformed.toPath());
		Files.writeString(new File(malformed, AbstractContentManager.META_JSON).toPath(), "{not json", StandardCharsets.UTF_8);
		// a stray file at the top of the store
		Files.writeString(new File(store, "stray.txt").toPath(), "stray", StandardCharsets.UTF_8);
		// a file where a content folder should be
		File third = new File(malformed.getParentFile().getParentFile(), "zz");
		Files.createDirectories(third.toPath());
		Files.writeString(new File(third, AbstractContentManager.META_JSON).toPath(), "{}", StandardCharsets.UTF_8);

		List<SearchResult> hits = all();

		assertEquals(1, hits.size());
		assertEquals(contentId, hits.get(0).getContentId());
	}

	@Test
	void anEmptyOrMissingStoreHasNoContent() throws Exception {
		ContentIterator missing = new FileSystemContentIterable().iterator();
		assertFalse(missing.hasNext());
		assertNull(missing.next());
		assertEquals(0L, missing.getTotalHits());

		Files.createDirectories(store().toPath());
		assertFalse(new FileSystemContentIterable().iterator().hasNext());
	}

	@Test
	void noContentWhenFileStorageIsOff() throws Exception {
		put("biz1", new Date());

		UtilImpl.CONTENT_FILE_STORAGE = false;

		assertFalse(new FileSystemContentIterable().iterator().hasNext());
	}

	@Test
	void totalHitsIsTheContentReturnedPlusOneWhileThereIsMore() throws Exception {
		for (int i = 0; i < 3; i++) {
			put("biz" + i, new Date());
		}

		ContentIterator it = new FileSystemContentIterable().iterator();
		assertEquals(1L, it.getTotalHits(), "before the walk");
		it.next();
		assertEquals(2L, it.getTotalHits(), "during the walk");
		it.next();
		it.next();
		assertFalse(it.hasNext());
		assertEquals(3L, it.getTotalHits(), "after the walk");
	}

	@Test
	void anAbandonedWalkDoesNotAffectTheNextOne() throws Exception {
		put("biz1", new Date());
		put("biz2", new Date());

		new FileSystemContentIterable().iterator().next();

		assertEquals(2, all().size());
	}

	@Test
	void contentRemovedByTheContentManagerIsNoLongerWalked() throws Exception {
		String removed = put("biz1", new Date());
		String kept = put("biz2", new Date());

		try (FileSystemContentManager manager = new FileSystemContentManager()) {
			manager.removeAttachment(removed);
		}

		List<SearchResult> hits = all();
		assertEquals(1, hits.size());
		assertEquals(kept, hits.get(0).getContentId());
	}

	private File store() {
		return tempDir.resolve(ContentManager.FILE_STORE_NAME).toFile();
	}

	private static String contentFolder(String contentId) {
		StringBuilder result = new StringBuilder(64);
		AbstractContentManager.appendBalancedFolderPathFromContentId(contentId, result);
		return result.toString();
	}

	private static String put(String bizId, Date lastModified) throws Exception {
		return put(bizId, lastModified, null);
	}

	private static String put(String bizId, Date lastModified, String contentId) throws Exception {
		AttachmentContent attachment = new AttachmentContent("demo", "admin", "User", "group1", "user1", bizId, "avatar")
				.attachment("avatar.txt", "text/plain", "hello".getBytes(StandardCharsets.UTF_8));
		attachment.setContentId(contentId);
		attachment.setLastModified(lastModified);
		try (FileSystemContentManager manager = new FileSystemContentManager()) {
			manager.put(attachment, false);
		}
		return attachment.getContentId();
	}

	private static List<SearchResult> all() throws Exception {
		List<SearchResult> result = new ArrayList<>();
		try (FileSystemContentManager manager = new FileSystemContentManager()) {
			for (SearchResult hit : manager.all()) {
				result.add(hit);
			}
		}
		return result;
	}
}
