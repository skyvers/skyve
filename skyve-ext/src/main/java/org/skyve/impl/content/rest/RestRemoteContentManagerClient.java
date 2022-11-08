package org.skyve.impl.content.rest;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResults;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;

/**
 * This class is used to talk to another skyve server's REST content server.
 * To use this, the contentManagerClass property of the factories property 
 * in the JSON file should be set to this class name and a "serverUrl" to the Skyve server should be defined.

 * @author mike
 * <p/>
 * something like this...
 * <p/>
 * <pre>
 *  JSON
 *  		...
 *			// Content settings
 *			"content": {
 *				// Skyve content server URL
 *				"serverUrl": "http://localhost:8080/skyve",
 *  		...
 *  		...
 *			// Factory settings
 *			factories: {
 *			...
 *				// Skyve content manager class
 *				contentManagerClass: "org.skyve.impl.content.rest.RestRemoteContentManagerClient"},
 *			...
 * </pre>
 */
public class RestRemoteContentManagerClient extends AbstractContentManager {
	private static final String REST_CONTENT_PATH = "/rest/content";
	
	@Override
	public void startup() {
		if (UtilImpl.CONTENT_REST_SERVER_URL == null) {
			throw new IllegalStateException("RestRemoteContentManagerClient is configured for the contentManager in the factories section of the json config but there is no content server URL defined in the content section.");
		}
	}

	@Override
	public void close() throws Exception {
		// nothing to do here
	}

	@Override
	public void shutdown() {
		// nothing to do here
	}

	@Override
	public void put(BeanContent content) throws Exception {
		Util.LOGGER.info("Remote call to RestRemoteContentManagerServer.put() sent for " + content.getBizId());
		StringBuilder url = new StringBuilder(128);
		url.append(UtilImpl.CONTENT_REST_SERVER_URL).append(REST_CONTENT_PATH).append(RestRemoteContentManagerServer.BEAN_PATH);
		call(url.toString(), "PUT", StateUtil.encode64(content));
	}

	@Override
	public void put(AttachmentContent content, boolean index) throws Exception {
		Util.LOGGER.info("Remote call to RestRemoteContentManagerServer.put() sent for " + content.getBizId() + " attribute " + content.getAttributeName());
		StringBuilder url = new StringBuilder(128);
		url.append(UtilImpl.CONTENT_REST_SERVER_URL).append(REST_CONTENT_PATH).append(RestRemoteContentManagerServer.ATTACHMENT_PATH);
		url.append("?index=").append(index);
		String contentId = call(url.toString(), "PUT", StateUtil.encode64(content));
		content.setContentId(contentId);
	}

	@Override
	public void update(AttachmentContent content) throws Exception {
		Util.LOGGER.info("Remote call to RestRemoteContentManagerServer.update() sent for " + content.getContentId());
		StringBuilder url = new StringBuilder(128);
		url.append(UtilImpl.CONTENT_REST_SERVER_URL).append(REST_CONTENT_PATH).append(RestRemoteContentManagerServer.ATTACHMENT_PATH);
		call(url.toString(), "POST", StateUtil.encode64(content));
	}
	
	@Override
	public AttachmentContent getAttachment(String contentId) throws Exception {
		Util.LOGGER.info("Remote call to RestRemoteContentManagerServer.getAttachment() sent for " + contentId);
		StringBuilder url = new StringBuilder(128);
		url.append(UtilImpl.CONTENT_REST_SERVER_URL).append(REST_CONTENT_PATH).append(RestRemoteContentManagerServer.ATTACHMENT_PATH).append('/').append(contentId);
		String result = call(url.toString(), "GET", null);
		return StateUtil.decode64(result);
	}

	@Override
	public void removeBean(String bizId) throws Exception {
		Util.LOGGER.info("Remote call to RestRemoteContentManagerServer.removeBean() sent for " + bizId);
		StringBuilder url = new StringBuilder(128);
		url.append(UtilImpl.CONTENT_REST_SERVER_URL).append(REST_CONTENT_PATH).append(RestRemoteContentManagerServer.BEAN_PATH).append('/').append(bizId);
		call(url.toString(), "DELETE", null);
	}

	@Override
	public void removeAttachment(String contentId) throws Exception {
		Util.LOGGER.info("Remote call to RestRemoteContentManagerServer.removeAttachment() sent for " + contentId);
		StringBuilder url = new StringBuilder(128);
		url.append(UtilImpl.CONTENT_REST_SERVER_URL).append(REST_CONTENT_PATH).append(RestRemoteContentManagerServer.ATTACHMENT_PATH).append('/').append(contentId);
		call(url.toString(), "DELETE", null);
	}

	private static String call(String url, String method, String body) {
		try {
			URL erl = new URL(url);
			HttpURLConnection connection = (HttpURLConnection) erl.openConnection();
			connection.setRequestMethod(method);

			if (body != null) {
				connection.setDoOutput(true);
				try (OutputStream os = connection.getOutputStream()) {
					os.write(body.getBytes(Util.UTF8));
					os.flush();
				}
			}

			connection.connect();
			int responseCode = connection.getResponseCode();
			if (responseCode != HttpURLConnection.HTTP_OK) {
				throw new IllegalStateException("Server response was " + responseCode);
			}
			
			try (InputStream is = connection.getInputStream()) {
				byte[] bytes = is.readAllBytes();
				return new String(bytes, Util.UTF8);
			}
		}
		catch (Exception e) {
			throw new DomainException("Cannot call content server at " + url, e);
		}
	}
	
	@Override
	public SearchResults google(String search, int maxResults) throws Exception {
		throw new UnsupportedOperationException("Google of a remote content repository is not supported");
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
