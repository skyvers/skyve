package org.skyve.impl.content;

import org.pf4j.Extension;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable;
import org.skyve.content.ContentManager;
import org.skyve.content.SearchResults;
import org.skyve.impl.content.lucene.LuceneContentManager;
import org.skyve.impl.util.UtilImpl;

@Extension(points = {ContentManager.class})
public class DelegatingContentManager extends AbstractContentManager {
	private AbstractContentManager delegate;
	
	private AbstractContentManager delegate() {
		if (delegate == null) {
			if (AbstractContentManager.IMPLEMENTATION_CLASS == null) {
				AbstractContentManager.IMPLEMENTATION_CLASS = LuceneContentManager.class;
			}
			delegate = AbstractContentManager.get();
		}
		return delegate;
	}
	
	@Override
	public void close() throws Exception {
		delegate().close();
	}

	@Override
	@SuppressWarnings("resource")
	public void put(BeanContent content) throws Exception {
		delegate().put(content);
	}

	@Override
	@SuppressWarnings("resource")
	public void put(AttachmentContent content, boolean index) throws Exception {
		delegate().put(content, index);
	}

	@Override
	@SuppressWarnings("resource")
	public void update(AttachmentContent content) throws Exception {
		delegate().update(content);
	}
	
	@Override
	@SuppressWarnings("resource")
	public AttachmentContent getAttachment(String contentId) throws Exception {
		return delegate().getAttachment(contentId);
	}

	@Override
	@SuppressWarnings("resource")
	public void removeBean(String bizId) throws Exception {
		delegate().removeBean(bizId);
	}

	@Override
	@SuppressWarnings("resource")
	public void removeAttachment(String contentId) throws Exception {
		delegate().removeAttachment(contentId);
	}

	@Override
	@SuppressWarnings("resource")
	public SearchResults google(String search, int maxResults) throws Exception {
		return delegate().google(search, maxResults);
	}

	@Override
	@SuppressWarnings("resource")
	public void truncate(String customerName) throws Exception {
		delegate().truncate(customerName);
	}

	@Override
	@SuppressWarnings("resource")
	public void truncateAttachments(String customerName) throws Exception {
		delegate().truncateAttachments(customerName);
	}

	@Override
	@SuppressWarnings("resource")
	public void truncateBeans(String customerName) throws Exception {
		delegate().truncateBeans(customerName);
	}

	@Override
	@SuppressWarnings("resource")
	public ContentIterable all() throws Exception {
		return delegate().all();
	}

	@Override
	@SuppressWarnings({"resource", "unchecked"})
	public void startup() {
		if (AbstractContentManager.IMPLEMENTATION_CLASS == null) {
			if (UtilImpl.SKYVE_CONTENT_MANAGER_CLASS != null) {
				try {
					AbstractContentManager.IMPLEMENTATION_CLASS = (Class<? extends AbstractContentManager>) Thread.currentThread().getContextClassLoader().loadClass(UtilImpl.SKYVE_CONTENT_MANAGER_CLASS);
				}
				catch (ClassNotFoundException e) {
					throw new IllegalStateException("Could not find factories.contentManagerClass " + UtilImpl.SKYVE_CONTENT_MANAGER_CLASS, e);
				}
			}
		}
		delegate().startup();
	}

	@Override
	@SuppressWarnings("resource")
	public void shutdown() {
		delegate().shutdown();
	}

	@Override
	@SuppressWarnings("resource")
	public void reindex(AttachmentContent attachment, boolean index) throws Exception {
		delegate().reindex(attachment, index);
	}
}
