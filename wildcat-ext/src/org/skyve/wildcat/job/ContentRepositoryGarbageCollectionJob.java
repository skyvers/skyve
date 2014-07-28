package org.skyve.wildcat.job;

import org.apache.jackrabbit.core.SessionImpl;
import org.apache.jackrabbit.core.data.GarbageCollector;
import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.util.Util;
import org.skyve.wildcat.content.ContentUtil;

public class ContentRepositoryGarbageCollectionJob implements Job {
	@Override
	public void execute(JobExecutionContext context)
	throws JobExecutionException {
		try {
			SessionImpl session = (SessionImpl) ContentUtil.getDefaultSession();
			try {
				GarbageCollector gc = session.createDataStoreGarbageCollector();
		
				gc.scan();
				gc.stopScan();
				gc.deleteUnused();
			}
			finally {
				if (session != null) {
					session.logout();
				}
			}

			Util.LOGGER.info("Successfully performed CMS garbage collection");
		}
		catch (Exception e) {
			throw new JobExecutionException("Error encountered whilst performing CMS garbage collection", e);
		}
	}
}
