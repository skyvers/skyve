package org.skyve.impl.content;

import java.io.File;
import java.io.InputStream;
import java.util.Date;
import java.util.UUID;

import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResult;
import org.skyve.content.SearchResults;
import org.skyve.content.TextExtractor;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.FileUtil;

/**
 * A content manager class that implements a file system storage but can't be searched or iterated.
 */
public class FileSystemContentManager extends AbstractContentManager {
	/**
	 * No-op for bean content; this manager only persists attachments on disk.
	 *
	 * @param content The bean content (ignored).
	 * @throws Exception Not thrown by this implementation.
	 */
	@Override
	public void put(BeanContent content) throws Exception {
		// nothing to do here
	}

	/**
	 * Store the attachment content on the file system and set metadata as needed.
	 *
	 * @param attachment The attachment to store.
	 * @param index Whether to index content (ignored for this implementation).
	 * @throws Exception If storage fails.
	 */
	@Override
	public void put(AttachmentContent attachment, boolean index) throws Exception {
		TextExtractor extractor = EXT.getAddInManager().getExtension(TextExtractor.class);
		if (extractor != null) {
			extractor.sniffContentType(attachment);
		}
		
		if (attachment.getContentId() == null) {
			attachment.setContentId(UUID.randomUUID().toString());
		}
		if (attachment.getLastModified() == null) {
			attachment.setLastModified(new Date());
		}
		if (UtilImpl.CONTENT_FILE_STORAGE) {
			StringBuilder absoluteContentStoreFolderPath = new StringBuilder(128);
			absoluteContentStoreFolderPath.append(UtilImpl.CONTENT_DIRECTORY).append(FILE_STORE_NAME).append('/');
			if (attachment.getExternalAbsoluteFilePath() != null) {
				writeExternalContentFile(absoluteContentStoreFolderPath, attachment);
			}
			else {
				try (InputStream content = attachment.getContentStream()) {
					writeContentFiles(absoluteContentStoreFolderPath, attachment, content, false);
				}
			}
		}
	}

	/**
	 * Update attachment metadata and persist the meta file when using file storage.
	 *
	 * @param attachment The attachment to update.
	 * @throws Exception If the metadata update fails.
	 */
	@Override
	public void update(AttachmentContent attachment) throws Exception {
		attachment.setLastModified(new Date());

		if (UtilImpl.CONTENT_FILE_STORAGE) {
			StringBuilder absoluteContentStoreFolderPath = new StringBuilder(128);
			absoluteContentStoreFolderPath.append(UtilImpl.CONTENT_DIRECTORY).append(FILE_STORE_NAME).append('/');
			String contentId = attachment.getContentId();
			AbstractContentManager.appendBalancedFolderPathFromContentId(contentId, absoluteContentStoreFolderPath);
			writeContentMeta(absoluteContentStoreFolderPath.toString(), attachment);
		}
	}
	
	/**
	 * Load an attachment from the file system.
	 *
	 * @param contentId The attachment content id.
	 * @return The attachment, or {@code null} when file storage is disabled.
	 * @throws Exception If loading fails.
	 */
	@Override
	public AttachmentContent getAttachment(String contentId) throws Exception {
		if (UtilImpl.CONTENT_FILE_STORAGE) {
			StringBuilder absoluteContentStoreFolderPath = new StringBuilder(128);
			absoluteContentStoreFolderPath.append(UtilImpl.CONTENT_DIRECTORY).append(FILE_STORE_NAME).append('/');
			return getFromFileSystem(absoluteContentStoreFolderPath, contentId, false);
		}
		return null;
	}

	/**
	 * No-op; bean content is not stored by this manager.
	 *
	 * @param bizId The bean business id (ignored).
	 * @throws Exception Not thrown by this implementation.
	 */
	@Override
	public void removeBean(String bizId) throws Exception {
		// nothing to do here
	}

	/**
	 * Remove attachment content and delete empty parent folders if possible.
	 *
	 * @param contentId The attachment content id to remove.
	 * @throws Exception If deletion fails.
	 */
	@Override
	@SuppressWarnings({"java:S3776", "java:S4042", "java:S1066", "java:S899"}) // complexity OK
	public void removeAttachment(String contentId) throws Exception {
		if (UtilImpl.CONTENT_FILE_STORAGE) {
			StringBuilder path = new StringBuilder(128);
			path.append(UtilImpl.CONTENT_DIRECTORY).append(FILE_STORE_NAME).append('/');
			AbstractContentManager.appendBalancedFolderPathFromContentId(contentId, path);
			File dir = new File(path.toString());
			File thirdDir = null;
			if (dir.exists()) {
				thirdDir = dir.getParentFile();
				FileUtil.delete(dir);
			}
			
			// Delete the folder structure housing the content file, if empty.
			if ((thirdDir != null) && thirdDir.exists() && thirdDir.isDirectory()) {
				File secondDir = thirdDir.getParentFile();
				if (thirdDir.delete()) {
					if ((secondDir != null) && secondDir.exists() && secondDir.isDirectory()) {
						File firstDir = secondDir.getParentFile();
						if (secondDir.delete()) {
							if ((firstDir != null) && firstDir.exists() && firstDir.isDirectory()) {
								firstDir.delete();
							}
						}
					}
				}
			}
		}
	}

	/**
	 * Search is not supported for file system content.
	 *
	 * @param search The search query (ignored).
	 * @param maxResults Maximum results to return (ignored).
	 * @return Empty results.
	 * @throws Exception Not thrown by this implementation.
	 */
	@Override
	public SearchResults google(String search, int maxResults) throws Exception {
		return new SearchResults();
	}

	/**
	 * No-op; indexing is not supported.
	 *
	 * @throws Exception Not thrown by this implementation.
	 */
	@Override
	public void dropIndexing() throws Exception {
		// do nothing
	}

	/**
	 * No-op; indexing is not supported.
	 *
	 * @param customerName The customer name (ignored).
	 * @throws Exception Not thrown by this implementation.
	 */
	@Override
	public void truncateIndexing(String customerName) throws Exception {
		// do nothing
	}

	/**
	 * No-op; indexing is not supported.
	 *
	 * @param customerName The customer name (ignored).
	 * @throws Exception Not thrown by this implementation.
	 */
	@Override
	public void truncateAttachmentIndexing(String customerName) throws Exception {
		// do nothing
	}

	/**
	 * No-op; indexing is not supported.
	 *
	 * @param customerName The customer name (ignored).
	 * @throws Exception Not thrown by this implementation.
	 */
	@Override
	public void truncateBeanIndexing(String customerName) throws Exception {
		// do nothing
	}

	/**
	 * Iteration is not supported; returns an empty iterable.
	 *
	 * @return A content iterable with no results.
	 * @throws Exception Not thrown by this implementation.
	 */
	@Override
	@SuppressWarnings("java:S1604") // Not a functional interface, but extends a functional interface
	public ContentIterable all() throws Exception {
		return new ContentIterable() {
			@Override
			public ContentIterator iterator() {
				return new ContentIterator() {
					@Override
					@SuppressWarnings("java:S2272") // No-op
					public SearchResult next() {
						return null;
					}
					
					@Override
					public boolean hasNext() {
						return false;
					}
					
					@Override
					public long getTotalHits() {
						return 0;
					}
				};
			}
		};
	}

	/**
	 * No-op; no resources to close.
	 *
	 * @throws Exception Not thrown by this implementation.
	 */
	@Override
	public void close() throws Exception {
		// do nothing
	}

	/**
	 * No-op initialization.
	 */
	@Override
	public void startup() {
		// do nothing
	}

	/**
	 * No-op shutdown.
	 */
	@Override
	public void shutdown() {
		// do nothing
	}

	/**
	 * No-op; indexing is not supported.
	 *
	 * @param attachment The attachment to reindex (ignored).
	 * @param index Whether to index content (ignored).
	 * @throws Exception Not thrown by this implementation.
	 */
	@Override
	public void reindex(AttachmentContent attachment, boolean index) throws Exception {
		// nothing to do here
	}
}
