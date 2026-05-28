package org.skyve.impl.content.jdbc;

import org.skyve.impl.content.lucene.LuceneContentManager;

/**
 * Combines Lucene content management with JDBC alias exposure for remote callers.
 *
 * <p>See {@link JDBCRemoteContentManagerServer} for remote JDBC setup details.
 * Configure this class when Lucene should back content storage.
 *
 * <p>Example configuration:
 * <pre>
 * // Skyve content manager class
 * contentManagerClass: "org.skyve.impl.content.jdbc.JDBCRemoteLuceneContentManagerServer"},
 * </pre>
 */
public class JDBCRemoteLuceneContentManagerServer extends LuceneContentManager {
	/**
	 * Starts JDBC alias exposure, then starts Lucene content services.
	 */
	@Override
	public void startup() {
		JDBCRemoteContentManagerServer.startup();
		super.startup();
	}
	
	/**
	 * Stops JDBC alias exposure and then shuts down Lucene content services.
	 */
	@Override
	public void shutdown() {
		try {
			JDBCRemoteContentManagerServer.shutdown();
		}
		finally {
			super.shutdown();
		}
	}
}
