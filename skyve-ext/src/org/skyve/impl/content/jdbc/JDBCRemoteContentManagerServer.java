package org.skyve.impl.content.jdbc;

import java.sql.CallableStatement;
import java.sql.Connection;

import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentManager;
import org.skyve.impl.content.elasticsearch.ESClient;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.StateUtil;
import org.skyve.util.Util;

/**
 * This class is used to expose the content server via JDBC to another skyve server.
 * To use this, the contentManager property of the factories property 
 * in the JSON file should be set to this class name and a "CONTENT" data store should be defined
 * as a local in-memory database.
 * 
 *  JSON
 *  		...
 *			// Factory settings
 *			factories: {
 *			...
 *			// Skyve content manager class
 *			contentManagerClass: "org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer"},
 *
 *			...
 *  		"CONTENT": {
 *				// JNDI name
 *				jndi: "java:/&lt;blahblah&gt;", 
 *				// Dialect
 *				dialect: "org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect"}
 *			...
 *
 * ds-xml URL
 * 
 * 		<connection-url>jdbc:h2:mem:content;IGNORECASE=TRUE;DB_CLOSE_DELAY=-1</connection-url>
 * 
 * @author mike
 */
public class JDBCRemoteContentManagerServer extends ESClient {
	static final String CONTENT_DATA_STORE_NAME = "CONTENT";
	static final String PUT_BEAN_FUNCTION_NAME = "PUT_BEAN";
	static final String PUT_ATTACHMENT_FUNCTION_NAME = "PUT_ATTACHMENT";
	static final String GET_ATTACHMENT_FUNCTION_NAME = "GET_ATTACHMENT";
	static final String REMOVE_BEAN_FUNCTION_NAME = "REMOVE_BEAN";
	static final String REMOVE_ATTACHMENT_FUNCTION_NAME = "REMOVE_ATTACHMENT";
	static final String GOOGLE_SEARCH_FUNCTION_NAME = "GOOGLE_SEARCH";
	
	@Override
	public void init() throws Exception {
		super.init();

		// register the database functions
		Util.LOGGER.info("REGISTER DATABASE FUNCTIONS FOR REMOTE CONTENT CALLS");
		try (Connection c = EXT.getDataStoreConnection(UtilImpl.DATA_STORES.get(CONTENT_DATA_STORE_NAME))) {
			try (CallableStatement s = c.prepareCall(String.format("DROP ALIAS IF EXISTS %s",
																	PUT_BEAN_FUNCTION_NAME))) {
				s.execute();
			}
			try (CallableStatement s = c.prepareCall(String.format("CREATE ALIAS %s FOR \"%s\"",
																	PUT_BEAN_FUNCTION_NAME,
																	"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer.putBean"))) {
				s.execute();
			}
			try (CallableStatement s = c.prepareCall(String.format("DROP ALIAS IF EXISTS %s",
																	PUT_ATTACHMENT_FUNCTION_NAME))) {
				s.execute();
			}
			try (CallableStatement s = c.prepareCall(String.format("CREATE ALIAS %s FOR \"%s\"",
																	PUT_ATTACHMENT_FUNCTION_NAME,
																	"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer.putAttachment"))) {
				s.execute();
			}
			try (CallableStatement s = c.prepareCall(String.format("DROP ALIAS IF EXISTS %s",
																	GET_ATTACHMENT_FUNCTION_NAME))) {
				s.execute();
			}
			try (CallableStatement s = c.prepareCall(String.format("CREATE ALIAS %s FOR \"%s\"",
																	GET_ATTACHMENT_FUNCTION_NAME,
																	"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer.getAttachment"))) {
				s.execute();
			}
			try (CallableStatement s = c.prepareCall(String.format("DROP ALIAS IF EXISTS %s",
																	REMOVE_BEAN_FUNCTION_NAME))) {
				s.execute();
			}
			try (CallableStatement s = c.prepareCall(String.format("CREATE ALIAS %s FOR \"%s\"",
																	REMOVE_BEAN_FUNCTION_NAME,
																	"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer.removeBean"))) {
				s.execute();
			}
			try (CallableStatement s = c.prepareCall(String.format("DROP ALIAS IF EXISTS %s",
																	REMOVE_ATTACHMENT_FUNCTION_NAME))) {
				s.execute();	
			}
			try (CallableStatement s = c.prepareCall(String.format("CREATE ALIAS %s FOR \"%s\"",
																	REMOVE_ATTACHMENT_FUNCTION_NAME,
																	"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer.removeAttachment"))) {
				s.execute();
			}
			try (CallableStatement s = c.prepareCall(String.format("DROP ALIAS IF EXISTS %s",
																	GOOGLE_SEARCH_FUNCTION_NAME))) {
				s.execute();	
			}
			try (CallableStatement s = c.prepareCall(String.format("CREATE ALIAS %s FOR \"%s\"",
																	GOOGLE_SEARCH_FUNCTION_NAME,
																	"org.skyve.impl.content.jdbc.JDBCRemoteContentManagerServer.googleSearch"))) {
				s.execute();
			}
		}
		Util.LOGGER.info("REGISTERED DATABASE FUNCTIONS FOR REMOTE CONTENT CALLS");
	}

	@Override
	public void dispose() throws Exception {
		super.dispose();

		// close the database if it wont automatically close
	}

	/*
	 * Data functions
	 */
	
	public static void putBean(String content) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			cm.put((BeanContent) StateUtil.decode64(content));
		}
	}
	
	public static void putAttachment(String content, boolean index) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			cm.put((AttachmentContent) StateUtil.decode64(content), index);
		}
	}

	public static String getAttachment(String id) throws Exception {
		String result = null;
		
		try (ContentManager cm = EXT.newContentManager()) {
			AttachmentContent content = cm.get(id);
			if (content != null) {
				result = StateUtil.encode64(content);
			}
		}
		
		return result;
	}

	public static void removeBean(String content) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			cm.remove((BeanContent) StateUtil.decode64(content));
		}
	}

	public static void removeAttachment(String contentId) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			cm.remove(contentId);
		}
	}
	
	public static String googleSearch(String search, int maxResults) throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			return StateUtil.encode64(cm.google(search, maxResults));
		}
	}
}
