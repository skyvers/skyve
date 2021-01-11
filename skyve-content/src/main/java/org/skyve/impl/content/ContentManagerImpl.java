package org.skyve.impl.content;

import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable;
import org.skyve.content.ContentManager;
import org.skyve.content.SearchResults;

public class ContentManagerImpl implements ContentManager {
	
	@Override
	public void close() throws Exception {
	}

	@Override
	public void put(BeanContent content) throws Exception {
	}

	@Override
	public void put(AttachmentContent content, boolean index) throws Exception {
	}

	@Override
	public AttachmentContent get(String id) throws Exception {
		return null;
	}

	@Override
	public void remove(BeanContent content) throws Exception {
	}

	@Override
	public void remove(String contentId) throws Exception {
	}

	@Override
	public SearchResults google(String search, int maxResults) throws Exception {
		return null;
	}

	@Override
	public void truncate(String customerName) throws Exception {
	}

	@Override
	public void truncateAttachments(String customerName) throws Exception {
	}

	@Override
	public void truncateBeans(String customerName) throws Exception {
	}

	@Override
	public ContentIterable all() throws Exception {
		return null;
	}
/*
	@Override
	public void init() throws Exception {
	}

	@Override
	public void dispose() throws Exception {
	}

	@Override
	public void reindex(AttachmentContent attachment, boolean index) throws Exception {
	}
*/

	@Override
	public void put(AttachmentContent content) throws Exception {
	}
}
