package org.skyve.impl.content;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Deque;
import java.util.Map;

import org.skyve.content.ContentIterable;
import org.skyve.content.ContentManager;
import org.skyve.content.SearchResult;
import org.skyve.domain.Bean;
import org.skyve.impl.util.TimeUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.FileUtil;
import org.skyve.util.JSON;
import org.skyve.util.logging.Category;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Iterates all attachments in the file system content store by walking its folders and reading each meta.json.
 *
 * <p>The store is laid out as {@code <aa>/<bb>/<cc>/<contentId>/} - see
 * {@link AbstractContentManager#appendBalancedFolderPathFromContentId(String, StringBuilder)}.
 * Each folder is listed when the walk reaches it, so no file handles are held between calls and an iteration
 * can be abandoned at any point. Folders are listed and meta.json files read without checking what they are first,
 * as each check is a round trip on a network share.
 *
 * <p>Complexity: O(n) file system reads where n is the number of content folders in the store; each
 * {@link #iterator()} walks the store again.
 */
public class FileSystemContentIterable implements ContentIterable {
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(FileSystemContentIterable.class);
	private static final Logger CONTENT_LOGGER = Category.CONTENT.logger();

	// The depth of the content ID folders below the store folder
	private static final int CONTENT_FOLDER_DEPTH = 4;

	private record Folder(@Nonnull File file, int depth) {
		// nothing to see here
	}

	/**
	 * Walks the store depth first in folder name order.
	 *
	 * <p>Not thread-safe.
	 */
	class FileSystemContentIterator implements ContentIterator {
		// Folders still to visit, the next one on top
		private final Deque<Folder> pending = new ArrayDeque<>();
		private SearchResult next = null;
		private long returned = 0L;
		private long skipped = 0L;
		private boolean finished = false;
		// Time spent listing folders and reading meta.json, excluding the caller's work between calls
		private long walkNanos = 0L;

		private FileSystemContentIterator() {
			pending.push(new Folder(new File(UtilImpl.CONTENT_DIRECTORY, ContentManager.FILE_STORE_NAME), 0));
		}

		/**
		 * Walks to the next content folder with a readable meta.json.
		 *
		 * <p>Side effects: logs the walk's folder count and file system time when the walk finishes.
		 *
		 * @return {@code true} when there is another attachment
		 */
		@Override
		public boolean hasNext() {
			long startNanos = System.nanoTime();
			while ((next == null) && (! pending.isEmpty())) {
				Folder folder = pending.pop();
				if (folder.depth() == CONTENT_FOLDER_DEPTH) {
					next = read(folder.file());
				}
				else {
					// push in reverse so the folders are visited in name order
					File[] children = list(folder.file());
					for (int i = children.length - 1; i >= 0; i--) {
						pending.push(new Folder(children[i], folder.depth() + 1));
					}
				}
			}
			walkNanos += System.nanoTime() - startNanos;

			if ((next == null) && (! finished)) {
				finished = true;
				LOGGER.info("Walked {} content folders ({} skipped) in {} seconds of file system time",
								Long.valueOf(returned + skipped),
								Long.valueOf(skipped),
								Long.valueOf(walkNanos / 1_000_000_000L));
			}
			return (next != null);
		}

		/**
		 * Returns the next attachment.
		 *
		 * @return the next attachment, or {@code null} when the walk has finished
		 */
		@Override
		@SuppressWarnings("java:S2272") // NoSuchElement exception not required as this should be in a for loop
		public SearchResult next() {
			if (! hasNext()) {
				return null;
			}
			SearchResult result = next;
			next = null;
			returned++;
			return result;
		}

		/**
		 * Returns a lower bound on the number of attachments: those returned so far, plus one if there is another.
		 * Counting the rest would walk the whole store, so a paged list pages forward one page at a time instead.
		 *
		 * @return the attachments returned so far, plus one if there is another
		 */
		@Override
		public long getTotalHits() {
			return returned + (hasNext() ? 1L : 0L);
		}

		/**
		 * Reads a content folder's meta.json the same way as
		 * {@link AbstractContentManager#getFromFileSystem(StringBuilder, String, boolean)}.
		 *
		 * @param contentFolder	a folder at the content ID depth of the store, which may be a stray file
		 * @return the search result for this content folder, or {@code null} if it has no readable meta.json
		 */
		private @Nullable SearchResult read(@Nonnull File contentFolder) {
			File metaFile = new File(contentFolder, AbstractContentManager.META_JSON);
			try {
				@SuppressWarnings("unchecked")
				Map<String, Object> meta = (Map<String, Object>) JSON.unmarshall(FileUtil.string(metaFile));

				SearchResult result = new SearchResult();
				result.setContentId(contentFolder.getName());
				result.setCustomerName((String) meta.get(Bean.CUSTOMER_NAME));
				result.setModuleName((String) meta.get(Bean.MODULE_KEY));
				result.setDocumentName((String) meta.get(Bean.DOCUMENT_KEY));
				result.setBizDataGroupId((String) meta.get(Bean.DATA_GROUP_ID));
				result.setBizUserId((String) meta.get(Bean.USER_ID));
				result.setBizId((String) meta.get(Bean.DOCUMENT_ID));
				result.setAttributeName((String) meta.get(AbstractContentManager.ATTRIBUTE_NAME));
				String lastModified = (String) meta.get(AbstractContentManager.LAST_MODIFIED);
				if (lastModified != null) {
					result.setLastModified(TimeUtil.parseISODate(lastModified));
				}
				return result;
			}
			catch (@SuppressWarnings("unused") FileNotFoundException e) { // no meta.json, or not a folder
				if (UtilImpl.CONTENT_TRACE) CONTENT_LOGGER.info("FileSystemContentIterable: Skipped {} - no meta.json", contentFolder);
				skipped++;
				return null;
			}
			catch (Exception e) { // IO, JSON, cast and date parse
				LOGGER.warn("Skipped {} - cannot read meta.json: {}", contentFolder, e.toString());
				skipped++;
				return null;
			}
		}
	}

	/**
	 * Returns a new walk of the store.
	 *
	 * @return a new iterator, which lists no folders until it is first used
	 */
	@Override
	public @Nonnull ContentIterator iterator() {
		return new FileSystemContentIterator();
	}

	/**
	 * Returns the entries of the given folder in name order.
	 *
	 * @param folder	the folder to list
	 * @return the entries, or none if it is a file, missing or not readable
	 */
	private static @Nonnull File[] list(@Nonnull File folder) {
		File[] result = folder.listFiles();
		if (result == null) {
			return new File[0];
		}
		Arrays.sort(result);
		return result;
	}
}
