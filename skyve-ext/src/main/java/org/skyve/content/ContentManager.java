package org.skyve.content;

import org.pf4j.ExtensionPoint;

public interface ContentManager extends AutoCloseable, ExtensionPoint {
	public static final String FILE_STORE_NAME = "SKYVE_STORE";

	public void put(BeanContent content) throws Exception;
	public void put(AttachmentContent content) throws Exception;
	public void put(AttachmentContent content, boolean index) throws Exception;
	public AttachmentContent get(String id) throws Exception;
	public void remove(BeanContent content) throws Exception;
	public void remove(String contentId) throws Exception;
	public SearchResults google(String search, int maxResults) throws Exception;
	public void truncate(String customerName) throws Exception;
	public void truncateAttachments(String customerName) throws Exception;
	public void truncateBeans(String customerName) throws Exception;
	public ContentIterable all() throws Exception;
}
