package org.skyve.impl.content.jdbc;

import org.skyve.impl.content.lucene.LuceneContentManager;

/**
 * See {@link JDBCRemoteContentManagerServer}.
 * Change the following line...
 * <pre>
 * // Skyve content manager class
 * contentManagerClass: "org.skyve.impl.content.jdbc.JDBCRemoteLuceneContentManagerServer"},
 * </pre>
 * @author mike
 */
public class JDBCRemoteLuceneContentManagerServer extends LuceneContentManager {
	@Override
	public void startup() {
		JDBCRemoteContentManagerServer.startup();
		super.startup();
	}
	
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
