package org.skyve.impl.content.ejb;

import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentManager;
import org.skyve.content.SearchResults;
import org.skyve.util.Util;

/**
 * Extend this to make a stateless session bean in the skyve server instance.
 * @author mike
 *
 * <p/>
 * Like this...<p/>
 * <code>
 * <pre>
 *	@Stateless
 *	public class EJBRemoteContentManagerServerBean extends org.skyve.impl.content.ejb.AbstractEJBRemoteContentManagerServerBean {
 *		// nothing to do here
 *	}
 * </pre>
 * </code>
 */
public abstract class AbstractEJBRemoteContentManagerServerBean implements EJBRemoteContentManagerServer {
	@Override
	public void put(BeanContent content) throws Exception {
		Util.LOGGER.info("Remote call to EJBRemoteContentManagerServer.put() received for " + content.getBizId());
		try (ContentManager cm = EXT.newContentManager()) {
			cm.put(content);
		}
	}
	
	@Override
	public String put(AttachmentContent content, boolean index) throws Exception {
		Util.LOGGER.info("Remote call to EJBRemoteContentManagerServer.put() received for " + content.getBizId() + " attribute " + content.getAttributeName());
		try (ContentManager cm = EXT.newContentManager()) {
			cm.put(content, index);
			return content.getContentId();
		}
	}

	@Override
	public AttachmentContent getAttachment(String contentId) throws Exception {
		Util.LOGGER.info("Remote call to EJBRemoteContentManagerServer.getAttachment() received for " + contentId);
		try (ContentManager cm = EXT.newContentManager()) {
			return cm.getAttachment(contentId);
		}
	}

	@Override
	public void removeBean(String bizId) throws Exception {
		Util.LOGGER.info("Remote call to EJBRemoteContentManagerServer.removeBean() received for " + bizId);
		try (ContentManager cm = EXT.newContentManager()) {
			cm.removeBean(bizId);
		}
	}

	@Override
	public void removeAttachment(String contentId) throws Exception {
		Util.LOGGER.info("Remote call to EJBRemoteContentManagerServer.removeAttachment() received for " + contentId);
		try (ContentManager cm = EXT.newContentManager()) {
			cm.removeAttachment(contentId);
		}
	}
	
	@Override
	public SearchResults google(String search, int maxResults) throws Exception {
		Util.LOGGER.info("Remote call to EJBRemoteContentManagerServer.google() received for '" + search + '\'');
		try (ContentManager cm = EXT.newContentManager()) {
			return cm.google(search, maxResults);
		}
	}
}
