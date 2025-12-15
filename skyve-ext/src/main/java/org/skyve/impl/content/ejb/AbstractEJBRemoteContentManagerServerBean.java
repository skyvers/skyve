package org.skyve.impl.content.ejb;

import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentManager;
import org.skyve.content.SearchResults;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Extend this to make a stateless session bean in the skyve server instance.
 * Not that all methods must be defined in this class for the @PermittAll annotation to have an effect.
 * @author mike
 *
 * <p/>
 * Like this...<p/>
 * <code>
 * <pre>
 *	@Stateless
 *	@Remote(EJBRemoteContentManagerServer.class)
 *	@TransactionAttribute(TransactionAttributeType.NEVER)
 *	@SecurityDomain("ejb")
 *	// NB to allow @PermittAll annotation to work, all methods permitted must be deined in this class - no polymorphism
 *	@PermitAll
 *	public class EJBRemoteContentManagerServerBean extends org.skyve.impl.content.ejb.AbstractEJBRemoteContentManagerServerBean {
 *		@Override
 *		public void put(BeanContent content) throws Exception {
 *			super.put(content);
 *		}
 *	
 *		@Override
 *		public String put(AttachmentContent content, boolean index) throws Exception {
 *			return super.put(content, index);
 *		}
 *
 *		@Override
 *		public void update(AttachmentContent content) throws Exception {
 *			super.update(content);
 *		}
 *	
 *		@Override
 *		public AttachmentContent getAttachment(String contentId) throws Exception {
 *			return super.getAttachment(contentId);
 *		}
 *
 *		@Override
 *		public void removeBean(String bizId) throws Exception {
 *			super.removeBean(bizId);
 *		}
 *
 *		@Override
 *		public void removeAttachment(String contentId) throws Exception {
 *			super.removeAttachment(contentId);
 *		}
 *	
 *		@Override
 *		public SearchResults google(String search, int maxResults) throws Exception {
 *			return super.google(search, maxResults);
 *		}
 *	}
 * </pre>
 * </code>
 * <p/>
 * This will require the following dependency for the SecurityDomain annotation
 *	<dependency>
 *		<groupId>org.jboss.ejb3</groupId>
 *			<artifactId>jboss-ejb3-ext-api</artifactId>
 *			<version>2.4.0.Final</version>
 *	</dependency>
 */
public abstract class AbstractEJBRemoteContentManagerServerBean implements EJBRemoteContentManagerServer {

    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractEJBRemoteContentManagerServerBean.class);

	@Override
	public void put(BeanContent content) throws Exception {
		LOGGER.info("Remote call to EJBRemoteContentManagerServer.put() received for {}", content.getBizId());
		try (ContentManager cm = EXT.newContentManager()) {
			cm.put(content);
		}
	}
	
	@Override
	public String put(AttachmentContent content, boolean index) throws Exception {
		LOGGER.info("Remote call to EJBRemoteContentManagerServer.put() received for {} attribute {}", content.getBizId(), content.getAttributeName());
		try (ContentManager cm = EXT.newContentManager()) {
			cm.put(content, index);
			return content.getContentId();
		}
	}

	@Override
	public void update(AttachmentContent content) throws Exception {
		LOGGER.info("Remote call to EJBRemoteContentManagerServer.update() received for {}", content.getContentId());
		try (ContentManager cm = EXT.newContentManager()) {
			cm.update(content);
		}
	}
	
	@Override
	public AttachmentContent getAttachment(String contentId) throws Exception {
		LOGGER.info("Remote call to EJBRemoteContentManagerServer.getAttachment() received for {}", contentId);
		try (ContentManager cm = EXT.newContentManager()) {
			return cm.getAttachment(contentId);
		}
	}

	@Override
	public void removeBean(String bizId) throws Exception {
		LOGGER.info("Remote call to EJBRemoteContentManagerServer.removeBean() received for {}", bizId);
		try (ContentManager cm = EXT.newContentManager()) {
			cm.removeBean(bizId);
		}
	}

	@Override
	public void removeAttachment(String contentId) throws Exception {
		LOGGER.info("Remote call to EJBRemoteContentManagerServer.removeAttachment() received for {}", contentId);
		try (ContentManager cm = EXT.newContentManager()) {
			cm.removeAttachment(contentId);
		}
	}
	
	@Override
	public SearchResults google(String search, int maxResults) throws Exception {
		LOGGER.info("Remote call to EJBRemoteContentManagerServer.google() received for '" + search + '\'');
		try (ContentManager cm = EXT.newContentManager()) {
			return cm.google(search, maxResults);
		}
	}
}
