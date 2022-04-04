package org.skyve.impl.content.ejb;

import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResults;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.util.Util;

/**
 * This class is used to talk to another skyve server's EJB content server.
 * Implement the server EJB lookup in obtainServer()
 * 
 * @author mike
 * <p/>
 * something like this...
 * <p/>
 * <code>
 * <pre>
 *	public EJBRemoteContentManagerServer obtainServer() throws Exception {
 *		Properties jndiProps = new Properties();
 *		jndiProps.put(Context.URL_PKG_PREFIXES, "org.jboss.ejb.client.naming");
 *		Context context = new InitialContext(jndiProps);
 *		// Lookup the content manager server bean using the ejb: namespace syntax which is explained here https://docs.jboss.org/author/display/AS71/EJB+invocations+from+a+remote+client+using+JNDI
 *		return (EJBRemoteContentManagerServer) context.lookup("ejb:skyve/apps//EJBRemoteContentManagerServerBean!org.skyve.impl.content.ejb.EJBRemoteContentManagerServer");
 *	}
 * </pre>
 * </code>
 * <pre>
 *  JSON
 *  		...
 *			// Factory settings
 *			factories: {
 *			...
 *			// Skyve content manager class
 *			contentManagerClass: "modules.MyEJBRemoteContentManagerClientImplementation"},
 *			...
 * </pre>
 */
public abstract class AbstractEJBRemoteContentManagerClient extends AbstractContentManager {
	@Override
	public void startup() {
		// nothing to do here
	}

	@Override
	public void close() throws Exception {
		// nothing to do here
	}

	@Override
	public void shutdown() {
		// nothing to do here
	}

	public abstract EJBRemoteContentManagerServer obtainServer() throws Exception;
	
	@Override
	public void put(BeanContent content) throws Exception {
		Util.LOGGER.info("Remote call to EJBRemoteContentManagerServer.put() sent for " + content.getBizId());
		EJBRemoteContentManagerServer server = obtainServer();
		server.put(content);
	}

	@Override
	public void put(AttachmentContent content, boolean index) throws Exception {
		Util.LOGGER.info("Remote call to EJBRemoteContentManagerServer.put() sent for " + content.getBizId() + " attribute " + content.getAttributeName());
		EJBRemoteContentManagerServer server = obtainServer();
		content.setContentId(server.put(content, index));
	}

	@Override
	public void update(AttachmentContent content) throws Exception {
		Util.LOGGER.info("Remote call to EJBRemoteContentManagerServer.update() sent for " + content.getContentId());
		EJBRemoteContentManagerServer server = obtainServer();
		server.update(content);
	}
	
	@Override
	public AttachmentContent getAttachment(String contentId) throws Exception {
		Util.LOGGER.info("Remote call to EJBRemoteContentManagerServer.getAttachment() sent for " + contentId);
		EJBRemoteContentManagerServer server = obtainServer();
		return server.getAttachment(contentId);
	}

	@Override
	public void removeBean(String bizId) throws Exception {
		Util.LOGGER.info("Remote call to EJBRemoteContentManagerServer.removeBean() sent for " + bizId);
		EJBRemoteContentManagerServer server = obtainServer();
		server.removeBean(bizId);
	}

	@Override
	public void removeAttachment(String contentId) throws Exception {
		Util.LOGGER.info("Remote call to EJBRemoteContentManagerServer.removeAttachment() sent for " + contentId);
		EJBRemoteContentManagerServer server = obtainServer();
		server.removeAttachment(contentId);
	}

	@Override
	public SearchResults google(String search, int maxResults) throws Exception {
		Util.LOGGER.info("Remote call to EJBRemoteContentManagerServer.google() sent for '" + search + '\'');
		EJBRemoteContentManagerServer server = obtainServer();
		return server.google(search, maxResults);
	}

	@Override
	public void truncate(String customerName) throws Exception {
		throw new UnsupportedOperationException("Truncate of a remote content repository is not supported");
	}

	@Override
	public void truncateAttachments(String customerName) throws Exception {
		throw new UnsupportedOperationException("Truncate of a remote content repository is not supported");
	}

	@Override
	public void truncateBeans(String customerName) throws Exception {
		throw new UnsupportedOperationException("Truncate of a remote content repository is not supported");
	}

	@Override
	public ContentIterable all() throws Exception {
		throw new UnsupportedOperationException("Iterating over a remote content repository is not supported");
	}
	
	@Override
	public void reindex(AttachmentContent attachment, boolean index) throws Exception {
		throw new UnsupportedOperationException("Reindexing a remote content repository is not supported");
	}
}
