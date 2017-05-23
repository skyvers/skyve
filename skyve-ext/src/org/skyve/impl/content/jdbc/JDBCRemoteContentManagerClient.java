package org.skyve.impl.content.jdbc;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.Types;

import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResults;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.StateUtil;

/**
 * This class is used to talk to another skyve server's content server via JDBC.
 * To use this, the contentManager property of the factories property 
 * in the JSON file should be set to this class name and a "CONTENT" data store should be defined
 * as an in-memory database.
 * 
 *  JSON
 *  		...
 *			// Factory settings
 *			factories: {
 *			...
 *			// Skyve content manager class
 *			contentManagerClass: "org.skyve.impl.content.jdbc.JDBCRemoteContentManagerClient"},
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
 * 		<connection-url>jdbc:h2:tcp://&lt;server-name&gt;/mem:content;IFEXISTS=TRUE;IGNORECASE=TRUE;DB_CLOSE_DELAY=-1</connection-url>
 * 		...
 *		<validation>
 *			<check-valid-connection-sql>select 1</check-valid-connection-sql>
 *			<validate-on-match>true</validate-on-match>
 *			<background-validation>false</background-validation>
 *			<background-validation-millis>0</background-validation-millis>
 *		</validation>
 *
 * @author mike
 */
public class JDBCRemoteContentManagerClient extends AbstractContentManager {
	@Override
	public void init() throws Exception {
		// nothing to do here
	}

	@Override
	public void close() throws Exception {
		// nothing to do here
	}

	@Override
	public void dispose() throws Exception {
		// nothing to do here
	}

	@Override
	public void put(BeanContent content) throws Exception {
		try (Connection c = EXT.getDataStoreConnection(UtilImpl.DATA_STORES.get(JDBCRemoteContentManagerServer.CONTENT_DATA_STORE_NAME))) {
			try (CallableStatement s = c.prepareCall(String.format("CALL %s(?)", JDBCRemoteContentManagerServer.PUT_BEAN_FUNCTION_NAME))) {
				s.setString(1, StateUtil.encode64(content));
				s.execute();
			}
		}
	}

	@Override
	public void put(AttachmentContent content, boolean index) throws Exception {
		try (Connection c = EXT.getDataStoreConnection(UtilImpl.DATA_STORES.get(JDBCRemoteContentManagerServer.CONTENT_DATA_STORE_NAME))) {
			try (CallableStatement s = c.prepareCall(String.format("? = CALL %s(?,?)", JDBCRemoteContentManagerServer.PUT_ATTACHMENT_FUNCTION_NAME))) {
				s.registerOutParameter(1, Types.VARCHAR);
				s.setString(2, StateUtil.encode64(content));
				s.setBoolean(3, index);
				s.execute();
				
				content.setContentId(s.getString(1));
			}
		}
	}

	@Override
	public AttachmentContent get(String id) throws Exception {
		AttachmentContent result = null;

		try (Connection c = EXT.getDataStoreConnection(UtilImpl.DATA_STORES.get(JDBCRemoteContentManagerServer.CONTENT_DATA_STORE_NAME))) {
			try (CallableStatement s = c.prepareCall(String.format("? = CALL %s(?)", JDBCRemoteContentManagerServer.GET_ATTACHMENT_FUNCTION_NAME))) {
				s.registerOutParameter(1, Types.CLOB);
				s.setString(2, id);
				s.execute();
				
				String payload = s.getString(1);
				if (payload != null) {
					result = StateUtil.decode64(payload);
				}
			}
		}
		
		return result;
	}

	@Override
	public void remove(BeanContent content) throws Exception {
		try (Connection c = EXT.getDataStoreConnection(UtilImpl.DATA_STORES.get(JDBCRemoteContentManagerServer.CONTENT_DATA_STORE_NAME))) {
			try (CallableStatement s = c.prepareCall(String.format("CALL %s(?)", JDBCRemoteContentManagerServer.REMOVE_BEAN_FUNCTION_NAME))) {
				s.setString(1, StateUtil.encode64(content));
				s.execute();
			}
		}
	}

	@Override
	public void remove(String contentId) throws Exception {
		try (Connection c = EXT.getDataStoreConnection(UtilImpl.DATA_STORES.get(JDBCRemoteContentManagerServer.CONTENT_DATA_STORE_NAME))) {
			try (CallableStatement s = c.prepareCall(String.format("CALL %s(?)", JDBCRemoteContentManagerServer.REMOVE_ATTACHMENT_FUNCTION_NAME))) {
				s.setString(1, contentId);
				s.execute();
			}
		}
	}

	@Override
	public SearchResults google(String search, int maxResults) throws Exception {
		SearchResults result = null;

		try (Connection c = EXT.getDataStoreConnection(UtilImpl.DATA_STORES.get(JDBCRemoteContentManagerServer.CONTENT_DATA_STORE_NAME))) {
			try (CallableStatement s = c.prepareCall(String.format("? = CALL %s(?,?)", JDBCRemoteContentManagerServer.GOOGLE_SEARCH_FUNCTION_NAME))) {
				s.registerOutParameter(1, Types.CLOB);
				s.setString(2, search);
				s.setInt(3, maxResults);
				s.execute();
				
				result = StateUtil.decode64(s.getString(1));
			}
		}
		
		return result;
	}

	@Override
	public void truncate(String customerName) throws Exception {
		throw new UnsupportedOperationException("Truncate of a remote content repository is not supported");
	}

	@Override
	public ContentIterable all() throws Exception {
		throw new UnsupportedOperationException("Iterating over a remote content repository is not supported");
	}
}
