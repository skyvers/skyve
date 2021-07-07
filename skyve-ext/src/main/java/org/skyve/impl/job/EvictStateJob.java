package org.skyve.impl.job;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.util.Util;

/**
 * This job evicts expired conversations from the conversations cache.
 * 
 * @author sandsm01
 */
public class EvictStateJob implements Job {
	
	@Override
	public void execute(JobExecutionContext context)
	throws JobExecutionException {
		try {
			Util.LOGGER.info("Evict expired conversations");
			StateUtil.evictExpiredConversations();
			Util.LOGGER.info("Successfully evicted expired conversations");
			Util.LOGGER.info("Evict expired session CSRF tokens");
			StateUtil.evictExpiredSessionTokens();
			Util.LOGGER.info("Successfully evicted expired session CSRF tokens");
		}
		catch (Exception e) {
			throw new JobExecutionException("Error encountered whilst evicting expired web state", e);
		}
	}
}