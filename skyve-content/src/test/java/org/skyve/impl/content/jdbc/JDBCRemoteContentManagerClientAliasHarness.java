package org.skyve.impl.content.jdbc;

import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.SearchResults;
import org.skyve.impl.cache.StateUtil;

public final class JDBCRemoteContentManagerClientAliasHarness {
	private JDBCRemoteContentManagerClientAliasHarness() {
	}

	public static final String TEST_CONTENT_ID = "remote-content-id";

	public static BeanContent lastPutBean;
	public static AttachmentContent lastPutAttachment;
	public static boolean lastPutAttachmentIndex;
	public static AttachmentContent lastUpdatedAttachment;
	public static String lastRequestedAttachmentId;
	public static String lastRemovedBeanId;
	public static String lastRemovedAttachmentId;
	public static String lastSearch;
	public static int lastMaxResults;

	public static void reset() {
		lastPutBean = null;
		lastPutAttachment = null;
		lastPutAttachmentIndex = false;
		lastUpdatedAttachment = null;
		lastRequestedAttachmentId = null;
		lastRemovedBeanId = null;
		lastRemovedAttachmentId = null;
		lastSearch = null;
		lastMaxResults = 0;
	}

	public static void putBeanAlias(String content) throws Exception {
		lastPutBean = StateUtil.decode64(content);
	}

	public static String putAttachmentAlias(String content, boolean index) throws Exception {
		lastPutAttachment = StateUtil.decode64(content);
		lastPutAttachmentIndex = index;
		return TEST_CONTENT_ID;
	}

	public static void updateAttachmentAlias(String content) throws Exception {
		lastUpdatedAttachment = StateUtil.decode64(content);
	}

	public static String getAttachmentAlias(String contentId) throws Exception {
		lastRequestedAttachmentId = contentId;
		if (lastPutAttachment == null) {
			return null;
		}
		return StateUtil.encode64(lastPutAttachment);
	}

	public static void removeBeanAlias(String bizId) {
		lastRemovedBeanId = bizId;
	}

	public static void removeAttachmentAlias(String contentId) {
		lastRemovedAttachmentId = contentId;
	}

	public static String googleSearchAlias(String search, int maxResults) throws Exception {
		lastSearch = search;
		lastMaxResults = maxResults;
		return StateUtil.encode64(new SearchResults());
	}
}