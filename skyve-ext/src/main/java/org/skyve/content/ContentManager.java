package org.skyve.content;

import org.pf4j.ExtensionPoint;
import org.skyve.impl.util.SystemObserver;

/**
 * Manages the storage, retrieval, search, and indexing of managed content
 * (file attachments and bean-level full-text search data).
 *
 * <p>Content is stored in a pluggable content store (the default implementation uses a
 * file-system store). Two categories of content are managed:
 * <ul>
 *   <li>{@link AttachmentContent} — binary file data attached to a document attribute.
 *       Stored by a content ID and optionally indexed for full-text search.</li>
 *   <li>{@link BeanContent} — textual metadata from a bean, indexed for search without
 *       storing a physical file.</li>
 * </ul>
 *
 * <p>Obtain a {@code ContentManager} via {@link org.skyve.EXT#newContentManager()}.
 * {@code ContentManager} implements {@link AutoCloseable}; always use it in a
 * try-with-resources block.
 *
 * <p>Threading: a {@code ContentManager} instance is not thread-safe. Use one
 * instance per request or execution context and close it when done.
 *
 * @see AttachmentContent
 * @see BeanContent
 * @see SearchResults
 */
public interface ContentManager extends AutoCloseable, ExtensionPoint, SystemObserver {
	public static final String FILE_STORE_NAME = "SKYVE_STORE";

	/**
	 * Put a bean in the content repository
	 * @param content
	 * @throws Exception
	 */
	void put(BeanContent content) throws Exception;

	/**
	 * Put (and index) an attachment in the content repository
	 * @param attachment
	 * @throws Exception
	 */
	default void put(AttachmentContent attachment) 
	throws Exception {
		put(attachment, true);
	}

	/**
	 * Put (and optionally index) an attachment in the content repository
	 * @param content
	 * @param index
	 * @throws Exception
	 */
	void put(AttachmentContent content, boolean index) throws Exception;

	/**
	 * Update an attachment in the content repository.
	 * This is used to update the content metadata for an attachment.
	 * This can be used when content is moved to a new data item in the database.
	 * @param content
	 * @throws Exception
	 */
	void update(AttachmentContent content) throws Exception;
	
	/**
	 * Get an attachment by content ID.
	 *
	 * @param contentId The unique attachment content identifier
	 * @return The attachment content, or {@code null} when not found
	 * @throws Exception If retrieval fails
	 */
	AttachmentContent getAttachment(String contentId) throws Exception;

	/**
	 * Remove a bean by bizId from the content repository.
	 * @param bizId
	 * @throws Exception
	 */
	void removeBean(String bizId) throws Exception;

	/**
	 * Remove an attachment by contentId from the content repository.
	 * @param contentId
	 * @throws Exception
	 */
	void removeAttachment(String contentId) throws Exception;

	/**
	 * Find matching content that the current user has access to containing the search term.
	 *
	 * @param search The search text/query string
	 * @param maxResults Maximum number of matches to return
	 * @return Search results visible to the current user
	 * @throws Exception If search execution fails
	 */
	SearchResults google(String search, int maxResults) throws Exception;
	
	/**
	 * Delete all indexing for all customers.
	 * Use this when indexing format changes.
	 * @throws Exception
	 */
	void dropIndexing() throws Exception;
	
	/**
	 * Remove all bean and attachment indexing for a customer.
	 * @param customerName
	 * @throws Exception
	 */
	void truncateIndexing(String customerName) throws Exception;

	/**
	 * Remove all attachment indexing for a customer.
	 * @param customerName
	 * @throws Exception
	 */
	void truncateAttachmentIndexing(String customerName) throws Exception;

	/**
	 * Remove all bean indexing for a customer.
	 * @param customerName
	 * @throws Exception
	 */
	void truncateBeanIndexing(String customerName) throws Exception;
	
	/**
	 * Iterate over all content independent of customer.
	 *
	 * @return Iterable cursor over all content records
	 * @throws Exception If iteration cannot be opened
	 */
	ContentIterable all() throws Exception;

	/**
	 * Flushes pending content changes and releases per-instance resources.
	 *
	 * <p>Idempotency: implementations must tolerate repeated calls on the same instance.
	 * A repeated call must not duplicate content or indexing side effects, and must not
	 * fail solely because resources have already been flushed or released. This supports
	 * persistence lifecycles that may flush content before serialising a conversation and
	 * later close the same persistence instance.
	 *
	 * @throws Exception if flushing or resource release fails for a reason other than
	 * already-completed cleanup
	 */
	@Override
	void close() throws Exception;
}
