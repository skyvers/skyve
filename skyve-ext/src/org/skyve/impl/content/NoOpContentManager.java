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
	public AttachmentContent get(String id) throws Exception {
		// no-op
		return null;
	}

	@Override
	public void remove(BeanContent content) throws Exception {
		// no-op
	}

	@Override
	public void remove(String contentId) throws Exception {
		// no-op
	}

	@Override
	public SearchResults google(String search, int maxResults) throws Exception {
		// no-op
		return null;
	}

	@Override
	public void truncate(String customerName) throws Exception {
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
	public void init() throws Exception {
		// no-op
	}

	@Override
	public void dispose() throws Exception {
		// no-op
	}

}
