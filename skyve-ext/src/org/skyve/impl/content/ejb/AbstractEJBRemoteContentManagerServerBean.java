package org.skyve.impl.content.ejb;

import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentManager;
import org.skyve.content.SearchResults;

/**
 * Extend this to make a stateless session bean in the skyve server instance.
 * <p/>
 * <code>
 * <pre>
 *	@Stateless
 *	public class EJBRemoteContentManagerServerBean extends org.skyve.impl.content.ejb.EJBRemoteContentManagerServerBean {
 *		// nothing to do here
 *	}
 * </pre>
 * </code>
 * 
 * @author mike
 */
public abstract class AbstractEJBRemoteContentManagerServerBean implements EJBRemoteContentManagerServer {
	@Override
	public void put(BeanContent content) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			cm.put(content);
		}
	}
	
	@Override
	public String put(AttachmentContent content, boolean index) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			cm.put(content, index);
			return content.getContentId();
		}
	}

	@Override
	public AttachmentContent get(String id) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			return cm.get(id);
		}
	}

	@Override
	public void remove(BeanContent content) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			cm.remove(content);
		}
	}

	@Override
	public void remove(String contentId) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			cm.remove(contentId);
		}
	}
	
	@Override
	public SearchResults google(String search, int maxResults) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			return cm.google(search, maxResults);
		}
	}
}
