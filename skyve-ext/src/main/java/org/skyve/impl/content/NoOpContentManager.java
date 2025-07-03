package org.skyve.impl.content;

import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResults;

public class NoOpContentManager extends AbstractContentManager {
	@Override
	public void put(BeanContent content) throws Exception {
		// no-op
	}

	@Override
	public void put(AttachmentContent content, boolean index) throws Exception {
		// no-op
	}

	@Override
	public void update(AttachmentContent content) throws Exception {
		// no-op
	}
	
	@Override
	public void reindex(AttachmentContent attachment, boolean index) throws Exception {
		// no-op
	}
	
	@Override
	public AttachmentContent getAttachment(String contentId) throws Exception {
		// no-op
		return null;
	}

	@Override
	public void removeBean(String bizId) throws Exception {
		// no-op
	}

	@Override
	public void removeAttachment(String contentId) throws Exception {
		// no-op
	}

	@Override
	public SearchResults google(String search, int maxResults) throws Exception {
		// no-op
		return null;
	}

	@Override
	public void dropIndexing() throws Exception {
		// no-op
	}
	
	@Override
	public void truncateIndexing(String customerName) throws Exception {
		// no-op
	}

	@Override
	public void truncateAttachmentIndexing(String customerName) throws Exception {
		// no-op
	}

	@Override
	public void truncateBeanIndexing(String customerName) throws Exception {
		// no-op
	}

	@Override
	public ContentIterable all() throws Exception {
		// no-op
		return null;
	}

	@Override
	public void close() throws Exception {
		// no-op
	}

	@Override
	public void startup() {
		// no-op
	}

	@Override
	public void shutdown() {
		// no-op
	}
}
