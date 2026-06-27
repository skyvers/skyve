package org.skyve.impl.content.jdbc;

import org.skyve.impl.content.elastic.ElasticContentManager;

/**
 * Combines Elasticsearch content management with JDBC alias exposure for remote callers.
 *
 * <p>See {@link JDBCRemoteContentManagerServer} for remote JDBC setup details.
 * Configure this class when Elasticsearch should back content storage.
 *
 * <p>Example configuration:
 * <pre>
 * // Skyve content manager class
 * contentManagerClass: "org.skyve.impl.content.jdbc.JDBCRemoteElasticContentManagerServer"},
 * </pre>
 */
public class JDBCRemoteElasticContentManagerServer extends ElasticContentManager {
	/**
	 * Starts JDBC alias exposure, then starts Elasticsearch content services.
	 */
	@Override
	public void startup() {
		JDBCRemoteContentManagerServer.startup();
		super.startup();
	}
	
	/**
	 * Stops JDBC alias exposure and then shuts down Elasticsearch content services.
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
