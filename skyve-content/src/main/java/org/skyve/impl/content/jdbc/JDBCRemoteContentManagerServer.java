package org.skyve.impl.content.jdbc;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.SQLException;

import org.h2.tools.Server;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentManager;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.util.UtilImpl;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

/**
 * Exposes content-manager operations as JDBC aliases for remote Skyve instances.
 *
 * <p>Configure this class as the active content manager on the serving node with a local
 * in-memory {@code CONTENT} datastore. A remote node can then call these aliases via
 * {@link JDBCRemoteContentManagerClient}.
 * 
 *  JSON
 *  		...
 *			// Content settings
 *			content: {
 *				// The arguments to send to the TCP server when running the content manager in server mode.
 *				serverArgs: "-tcpPort 9092 -tcpAllowOthers -ifExists"
 *			}
 *			...
 *			// Factory settings
 *			factories: {
 *			...
 *				// Skyve content manager class
 *				contentManagerClass: "org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer"},
 *
 *			...
 *			// Datastore definitions
 *			dataStores: {
 *			...
 *				"CONTENT": {
 *					// JNDI name
 *					jndi: "java:/&lt;blahblah&gt;", 
 *					// Dialect
 *					dialect: "org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect"}
 *			...
 *
 * ds-xml URL
 * 
 * 		<connection-url>jdbc:h2:mem:content;IGNORECASE=TRUE;DB_CLOSE_DELAY=-1</connection-url>
 * 
 * <p>Threading: static utility style; server lifecycle is process-scoped.
 */
public class JDBCRemoteContentManagerServer {
	static final String CONTENT_DATA_STORE_NAME = "CONTENT";
	static final String PUT_BEAN_FUNCTION_NAME = "PUT_BEAN";
	static final String PUT_ATTACHMENT_FUNCTION_NAME = "PUT_ATTACHMENT";
	static final String UPDATE_ATTACHMENT_FUNCTION_NAME = "UPDATE_ATTACHMENT";
	static final String GET_ATTACHMENT_FUNCTION_NAME = "GET_ATTACHMENT";
	static final String REMOVE_BEAN_FUNCTION_NAME = "REMOVE_BEAN";
	static final String REMOVE_ATTACHMENT_FUNCTION_NAME = "REMOVE_ATTACHMENT";
	static final String GOOGLE_SEARCH_FUNCTION_NAME = "GOOGLE_SEARCH";

    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(JDBCRemoteContentManagerServer.class);

	private static Server server = null;

	/**
	 * Prevents instantiation.
	 */
	private JDBCRemoteContentManagerServer() {
		// nothing to do here
	}
	
	/**
	 * Starts the H2 TCP server and registers content-manager aliases.
	 *
	 * @throws IllegalStateException if startup or alias registration fails
	 */
	public static void startup() {
		// Start TCP server
		if (UtilImpl.CONTENT_JDBC_SERVER_ARGS == null) {
			throw new IllegalStateException("JDBCRemoteContentManagerServer is configured for the contentManager in the factories section of the json config but there are no server arguments defined in the content section.");
		}

		try {
			server = Server.createTcpServer(UtilImpl.CONTENT_JDBC_SERVER_ARGS.split("\\s+")).start();
			
			// register the database functions
			LOGGER.info("REGISTER DATABASE FUNCTIONS FOR REMOTE CONTENT CALLS");
			try (Connection c = EXT.getDataStoreConnection(UtilImpl.DATA_STORES.get(CONTENT_DATA_STORE_NAME), false)) {
				try (CallableStatement s = c.prepareCall(String.format("DROP ALIAS IF EXISTS %s",
																		PUT_BEAN_FUNCTION_NAME))) {
					s.execute();
				}
				try (CallableStatement s = c.prepareCall(String.format("CREATE ALIAS %s FOR \"%s\"",
																		PUT_BEAN_FUNCTION_NAME,
																		"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer.putBeanFunction"))) {
					s.execute();
				}
				try (CallableStatement s = c.prepareCall(String.format("DROP ALIAS IF EXISTS %s",
																		PUT_ATTACHMENT_FUNCTION_NAME))) {
					s.execute();
				}
				try (CallableStatement s = c.prepareCall(String.format("CREATE ALIAS %s FOR \"%s\"",
																		PUT_ATTACHMENT_FUNCTION_NAME,
																		"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer.putAttachmentFunction"))) {
					s.execute();
				}
				try (CallableStatement s = c.prepareCall(String.format("DROP ALIAS IF EXISTS %s",
																		UPDATE_ATTACHMENT_FUNCTION_NAME))) {
					s.execute();
				}
				try (CallableStatement s = c.prepareCall(String.format("CREATE ALIAS %s FOR \"%s\"",
																		UPDATE_ATTACHMENT_FUNCTION_NAME,
																		"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer.updateAttachmentFunction"))) {
					s.execute();
				}
				try (CallableStatement s = c.prepareCall(String.format("DROP ALIAS IF EXISTS %s",
																		GET_ATTACHMENT_FUNCTION_NAME))) {
					s.execute();
				}
				try (CallableStatement s = c.prepareCall(String.format("CREATE ALIAS %s FOR \"%s\"",
																		GET_ATTACHMENT_FUNCTION_NAME,
																		"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer.getAttachmentFunction"))) {
					s.execute();
				}
				try (CallableStatement s = c.prepareCall(String.format("DROP ALIAS IF EXISTS %s",
																		REMOVE_BEAN_FUNCTION_NAME))) {
					s.execute();
				}
				try (CallableStatement s = c.prepareCall(String.format("CREATE ALIAS %s FOR \"%s\"",
																		REMOVE_BEAN_FUNCTION_NAME,
																		"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer.removeBeanFunction"))) {
					s.execute();
				}
				try (CallableStatement s = c.prepareCall(String.format("DROP ALIAS IF EXISTS %s",
																		REMOVE_ATTACHMENT_FUNCTION_NAME))) {
					s.execute();	
				}
				try (CallableStatement s = c.prepareCall(String.format("CREATE ALIAS %s FOR \"%s\"",
																		REMOVE_ATTACHMENT_FUNCTION_NAME,
																		"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer.removeAttachmentFunction"))) {
					s.execute();
				}
				try (CallableStatement s = c.prepareCall(String.format("DROP ALIAS IF EXISTS %s",
																		GOOGLE_SEARCH_FUNCTION_NAME))) {
					s.execute();	
				}
				try (CallableStatement s = c.prepareCall(String.format("CREATE ALIAS %s FOR \"%s\"",
																		GOOGLE_SEARCH_FUNCTION_NAME,
																		"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer.googleSearchFunction"))) {
					s.execute();
				}
			}
			LOGGER.info("REGISTERED DATABASE FUNCTIONS FOR REMOTE CONTENT CALLS");
		}
		catch (SQLException e) {
			throw new IllegalStateException("Could not startup JDBCRemoteContentManagerServer", e);
		}
	}

	/**
	 * Stops the H2 TCP server if it is running.
	 */
	public static void shutdown() {
		// close the database if it wont automatically close
		if (server != null) {
			server.stop();
			server = null;
		}
	}

	/*
	 * Data functions
	 */
	
	/**
	 * Decodes and stores bean content from a remote caller.
	 *
	 * @param content base64 encoded serialized {@link BeanContent}
	 * @throws Exception if decoding or storage fails
	 */
	public static void putBeanFunction(String content) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			cm.put((BeanContent) StateUtil.decode64(content));
		}
	}
	
	/**
	 * Decodes and stores attachment content from a remote caller.
	 *
	 * @param content base64 encoded serialized {@link AttachmentContent}
	 * @param index whether textual indexing should be performed
	 * @return the assigned content identifier
	 * @throws Exception if decoding or storage fails
	 */
	public static String putAttachmentFunction(String content, boolean index) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			AttachmentContent attachment = (AttachmentContent) StateUtil.decode64(content);
			cm.put(attachment, index);
			return attachment.getContentId();
		}
	}

	/**
	 * Decodes and updates existing attachment content from a remote caller.
	 *
	 * @param content base64 encoded serialized {@link AttachmentContent} update
	 * @throws Exception if decoding or update fails
	 */
	public static void updateAttachmentFunction(String content) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			AttachmentContent attachment = (AttachmentContent) StateUtil.decode64(content);
			cm.update(attachment);
		}
	}

	/**
	 * Retrieves and serializes attachment content for a remote caller.
	 *
	 * @param contentId the content identifier
	 * @return base64 encoded serialized {@link AttachmentContent}, or {@code null} when absent
	 * @throws Exception if retrieval or serialization fails
	 */
	public static String getAttachmentFunction(String contentId) throws Exception {
		String result = null;
		
		try (ContentManager cm = EXT.newContentManager()) {
			AttachmentContent content = cm.getAttachment(contentId);
			if (content != null) {
				result = StateUtil.encode64(content);
			}
		}
		
		return result;
	}

	/**
	 * Removes all bean-scoped indexed content for the supplied business identifier.
	 *
	 * @param bizId the business identifier
	 * @throws Exception if removal fails
	 */
	public static void removeBeanFunction(String bizId) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			cm.removeBean(bizId);
		}
	}

	/**
	 * Removes an attachment content record by content identifier.
	 *
	 * @param contentId the content identifier
	 * @throws Exception if removal fails
	 */
	public static void removeAttachmentFunction(String contentId) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			cm.removeAttachment(contentId);
		}
	}
	
	/**
	 * Executes full-text search and serializes results for remote callers.
	 *
	 * @param search the search expression
	 * @param maxResults maximum number of results to return
	 * @return base64 encoded serialized search results
	 * @throws Exception if query execution or serialization fails
	 */
	public static String googleSearchFunction(String search, int maxResults) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			return StateUtil.encode64(cm.google(search, maxResults));
		}
	}
}
