package org.skyve.content;

import org.pf4j.ExtensionPoint;
import org.skyve.impl.util.SystemObserver;

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
	 * @param id
	 * @return
	 * @throws Exception
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
	 * @param search
	 * @param maxResults
	 * @return
	 * @throws Exception
	 */
	SearchResults google(String search, int maxResults) throws Exception;
	
	/**
	 * Remove all beans and attachments for a customer.
	 * @param customerName
	 * @throws Exception
	 */
	void truncate(String customerName) throws Exception;

	/**
	 * Remove all attachments for a customer.
	 * @param customerName
	 * @throws Exception
	 */
	void truncateAttachments(String customerName) throws Exception;

	/**
	 * Remove all beans for a customer.
	 * @param customerName
	 * @throws Exception
	 */
	void truncateBeans(String customerName) throws Exception;
	
	/**
	 * Iterate over all content independent of customer.
	 * @return
	 * @throws Exception
	 */
	ContentIterable all() throws Exception;
}
